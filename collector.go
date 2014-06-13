package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"os/signal"
	"strconv"
	"syscall"
	"time"

	"github.com/anchor/vaultaire-collector-nagios/perfdata"
)

const (
	Version = "1.0.2"
)

const DefaultParallelism = 10

// Metrics about the collector run to be reported in the log at the end.
var RunMetrics *CollectorMetricData

// logTermination outputs metrics on collector exit.
func logTermination(startTime time.Time) {
	addRuntimeMetric(startTime)
	// Output metrics in format designed for machine parsing. Try to
	// avoid breaking this - these lines should match the regex
	// /metrics:([a-z_]+=[\d.])+/, with one metric per line.
	for _, metric := range RunMetrics.Metrics() {
		Log.Infof("%v", metric)
	}
}

// handleSignals catches the usual exit signals and logs some data
// before exiting.
//
// We don't call cleanup here because we want to be able to kill the
// collector with a SIGTERM whether or not it's finished writing; this
// may change once there's better monitoring in place.
func handleSignals(startTime time.Time) {
	c := make(chan os.Signal, 3)
	signal.Notify(c, syscall.SIGINT, syscall.SIGTERM, syscall.SIGSEGV)
	go func() {
		for sig := range c {
			Log.Errorf("Caught %v, shutting down.", sig)
			Log.Errorf("Frames may not have been flushed by libmarquise.")
			logTermination(startTime)
			os.Exit(1)
		}
	}()
}

// dieIfTimedOut checks whether more than timeout milliseconds has
// elapsed, and logs an error and dies if this is the case.
func dieIfTimedOut(startTime time.Time, timeout time.Duration) {
	now := time.Now()
	delta := now.Sub(startTime)
	if delta > timeout {
		Log.Errorf("Timeout (%v) exceeded, terminating.", timeout)
		logTermination(startTime)
		os.Exit(2)
	}
}

func addRuntimeMetric(startTime time.Time) {
	now := time.Now()
	dTime := now.Sub(startTime)
	dSeconds := float64(dTime) / float64(time.Second)
	// Format with smallest unique representation that doesn't
	// involve scientific notation.
	value := strconv.FormatFloat(dSeconds, 'f', -1, 64)
	RunMetrics.AddMetric("runtime", value)
}

func cleanup(startTime time.Time, writer PerfDatumWriter) {
	writer.Shutdown()
	logTermination(startTime)
}

func main() {
	startTime := time.Now()
	RunMetrics = NewCollectorMetricData()
	configFile := flag.String("cfg", "/etc/vaultaire/nagios.gcfg", "Path to configuration file. This file should be in gcfg[0] format. [0] https://code.google.com/p/gcfg/")
	burstMode := flag.Bool("burst-mode", false, "Write all input as "+
		"a single DataBurst to stdout and then exit. Don't try this in "+
		"production.")

	flag.Usage = func() {
		helpMessage := "vaultaire-collector-nagios will write " +
			"Nagios-format perfdata (passed on stdin) to vaultaire\n\n" +
			fmt.Sprintf("Usage: %s [options]\n\n", os.Args[0]) +
			"Options:\n\n"
		fmt.Fprintf(os.Stderr, helpMessage)
		flag.PrintDefaults()
	}
	flag.Parse()

	// If we're in burst mode we don't need logging, parallelism or
	// anything else fancy. Just run and exit.
	if *burstMode {
		err := runBurstMode()
		if err != nil {
			log.Fatal(err)
		}
		os.Exit(0)
	}

	handleSignals(startTime)

	cfgPath := *configFile
	cfg, err := InitializeConfig(cfgPath)
	if err != nil {
		log.Fatalf("Could not initialize config from file %v: %v", cfgPath, err)
	}

	parallelism := DefaultParallelism
	if cfg.General.Parallelism > 0 {
		parallelism = cfg.General.Parallelism
	}

	Log.Debugf("Using parallelism: %v", parallelism)
	var writer PerfDatumWriter

	if cfg.General.StorageBackend == "file" {
		writer, err = NewBletchleyDataFrameFileWriter(cfg.File.DataFrameFile)
	} else if cfg.General.StorageBackend == "vaultaire" {
		writer, err = NewBletchleyDataFrameVaultaireWriter(cfg.Vaultaire.Broker, cfg.Vaultaire.BatchPeriod, cfg.Vaultaire.Origin, cfg.Vaultaire.TelemetryEndpoint, cfg.Vaultaire.MarquiseDebug)
	}

	if err != nil {
		Log.Fatalf("Couldn't initialize writer: %v", err)
	}

	defer cleanup(startTime, writer)

	timeout := time.Duration(cfg.General.Timeout) * time.Millisecond
	go func() {
		for true {
			dieIfTimedOut(startTime, timeout)
			time.Sleep(time.Millisecond)
		}
	}()
	perfLines := perfdata.ReadPerfDataRecords(os.Stdin)

	resultChannels := make([]chan PerfDataWriteResult, 0)
	semaphore := make(chan int, parallelism)

	for i := 0; i < parallelism; i++ {
		semaphore <- 1
	}

	for _, line := range perfLines {
		result := make(chan PerfDataWriteResult)
		resultChannels = append(resultChannels, result)
		go processPerfDataRecord(result, semaphore, writer, line)
	}

	recordsWritten := 0
	// Set now so it's reported if we die mid-run.
	RunMetrics.AddMetric("records_written", "0")
	var totalWriteTime, nWriteTimes int64
	for _, ch := range resultChannels {
		result := <-ch
		recordsWritten += result.RecordsWritten
		for _, writeTime := range result.Times {
			totalWriteTime += writeTime
			nWriteTimes += 1
		}
	}

	Log.Infof("Wrote %v records.", recordsWritten)
	recordsMetric := fmt.Sprintf("%v", recordsWritten)
	RunMetrics.AddMetric("records_written", recordsMetric)

	Log.Debugf("Total time spent in writes (seconds): %v", float64(totalWriteTime)/1000000000.0)
	Log.Debugf("Average time spent in writes (seconds): %v", float64(totalWriteTime)/float64(nWriteTimes)/1000000000.0)

}
