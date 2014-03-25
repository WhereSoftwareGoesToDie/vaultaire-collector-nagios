package main

import (
	"github.com/anchor/vaultaire-collector-nagios/perfdata"
)

type PerfDatumWriter interface {
	Write(perfdata.RenderedPerfDatumValue) error
	Shutdown() error
}
