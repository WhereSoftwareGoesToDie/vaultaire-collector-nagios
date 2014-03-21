package main

// Config defines the configuration file format for
// vaultaire-collector-nagios.
type Config struct {
	General struct {
		// Writes to execute in parallel.
		Parallelism int
		// Writes to perform as one operation (where
		// applicable).
		BatchSize      int
		StorageBackend string
		LogFile        string
		LogLevel       string
		// Kill ourselves if we run longer than this 
		// (milliseconds).
		Timeout int
	}
	Vaultaire struct {
		Broker        string
		BatchPeriod   float64
		Origin        string
		MarquiseDebug bool
		TelemetryEndpoint string
	}
	File struct {
		DataFrameFile string
	}
}
