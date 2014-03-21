package main

import (
	"github.com/anchor/bletchley/perfdata"
	"time"
)

// PerfDataWriteResult holds write statistics for reporting purposes.
type PerfDataWriteResult struct {
	RecordsWritten int
	Times          []int64
	Failed         bool
}

// processPerfDataRecord takes a slice of 'KEY::VALUE' pairs from one
// line of perfdata, and does the following:
//
// - parses them as either a service check result or a host check
//   result
// - extracts the individual metrics from the result
// - writes each metric to the supplied PerfDatumWriter
// - writes throughput statistics to the `written` channel
//
// This function is designed to be called in parallel, so it blocks on
// reading from a semaphore channel before proceeding.
func processPerfDataRecord(written chan PerfDataWriteResult, semaphore chan int, writer PerfDatumWriter, line []string) {
	<-semaphore
	writeResult := new(PerfDataWriteResult)
	datum, err := perfdata.NewPerfDatum(line)
	if err != nil {
		Log.Errorf("Error parsing record: %v", err)
		writeResult.Failed = true
		written <- *writeResult
		semaphore <- 1
		return
	}
	// Record parsed, extract the individual perfdata
	metrics, err := datum.RenderMetrics()
	if err != nil {
		Log.Errorf("Could not extract perfdata: %v", err)
		writeResult.Failed = true
		written <- *writeResult
		semaphore <- 1
		return
	}
	// Got everything we need, now write it to our storage backend
	for _, metricRecord := range metrics {
		preTime := time.Now()
		err := writer.Write(metricRecord)
		postTime := time.Now()
		writeTime := postTime.UnixNano() - preTime.UnixNano()
		writeSeconds := float64(writeTime) / 1000000000.0
		Log.Debugf("Write took %v seconds.", writeSeconds)
		writeResult.Times = append(writeResult.Times, writeTime)
		if err != nil {
			Log.Errorf("Failed to write %v: %v", metricRecord.GetKey(), err)
		} else {
			writeResult.RecordsWritten += 1
		}
	}
	written <- *writeResult
	semaphore <- 1
}
