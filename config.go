package main

// Config defines the configuration file format for
// vaultaire-collector-nagios.
type Config struct {
	General struct {
		LogFile        string
		LogLevel       string
		// Kill ourselves if we run longer than this
		// (milliseconds).
		Timeout int
	}
	Vaultaire struct {
		// Namespace for the Marquise daemon.
		Namespace string
	}
}
