package perfdata

import (
	"testing"
)

const (
	numPerfDataLines = 6
)

func TestReadPerfdata(t *testing.T) {
	file := OpenTestPerfdataFile(t)
	perfLines := ReadPerfDataRecords(file)
	if len(perfLines) != numPerfDataLines {
		t.Errorf("Line count mismatch: expected %v perfdata lines, read %v perfdata lines.", numPerfDataLines, len(perfLines))
	}
}
