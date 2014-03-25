package perfdata

import (
	"bufio"
	"encoding/csv"
	"io"
	"os"
)

func ReadPerfDataRecords(stream *os.File) [][]string {
	fReader := bufio.NewReader(stream)
	tsvReader := csv.NewReader(fReader)
	tsvReader.Comma = '\t'
	tsvReader.LazyQuotes = true

	lines := make([][]string, 0)

	for {
		line, err := tsvReader.Read()
		if err == io.EOF {
			break
		}
		// FIXME: need better error handling here. Probably
		// shouldn't break on every malformed line, but need to
		// know if we can't read anything at all.
		lines = append(lines, line)
	}
	return lines
}
