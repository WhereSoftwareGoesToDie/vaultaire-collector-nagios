package main

import (
	"github.com/anchor/bletchley/perfdata"
)

type PerfDatumWriter interface {
	Write(perfdata.RenderedPerfDatumValue) error
	Shutdown() error
}
