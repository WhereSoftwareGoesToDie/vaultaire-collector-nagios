package main

import (
	"github.com/anchor/vaultaire-collector-nagios/perfdata"
	"os"
	"testing"
)

func OpenTestPerfdataFile(t *testing.T) *os.File {
	stream, err := os.Open("testdata/test-perfdata")
	if err != nil {
		t.Errorf("Could not open testdata: %v", err)
	}
	return stream
}

func RenderTestPerfdata(t *testing.T, perfLines [][]string) [][]perfdata.RenderedPerfDatumValue {
	renderedData := make([][]perfdata.RenderedPerfDatumValue, 0)
	for i, line := range perfLines {
		datum, err := perfdata.NewPerfDatum(line)
		if err != nil {
			t.Errorf("Failed to parse line %v in perfdata file (%v): %v", i, line, err)
		}
		rendered, err := datum.RenderMetrics()
		if err != nil {
			t.Errorf("Failed to render datum on line %v in perfdata file (%v): %v", i, datum, err)
		}
		renderedData = append(renderedData, rendered)
	}
	return renderedData
}
