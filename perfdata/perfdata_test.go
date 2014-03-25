package perfdata

import (
	"testing"
)

func TestParsePerfdata(t *testing.T) {
	file := OpenTestPerfdataFile(t)
	perfLines := ReadPerfDataRecords(file)
	renderedData := RenderTestPerfdata(t, perfLines)
	sampleRenderedDatum := RenderedPerfDatumValue{"overall_write_count", "6497849c", uint64(1388445486), "diskio", "node4.example.com"}
	if renderedData[3][1] != sampleRenderedDatum {
		t.Errorf("Expected rendered values %v, got %v instead", sampleRenderedDatum, renderedData[3][1])
	}
}
