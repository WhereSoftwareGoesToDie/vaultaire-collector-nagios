package perfdata

import (
	"github.com/anchor/dataframe"
	"strconv"
	"time"
)

const (
	BletchleyDataSourcePrefix = "perfdata/nagios"
)

// Return a struct of key/value pairs in the form of a slice of pointers
// to dataframe.DataFrame_Tags.
func getDataSource(datum RenderedPerfDatumValue, unit UOM) []*dataframe.DataFrame_Tag {
	source := make([]*dataframe.DataFrame_Tag, 0)
	source = append(source, dataframe.NewDataFrameTag("origin", BletchleyDataSourcePrefix))
	source = append(source, dataframe.NewDataFrameTag("hostname", datum.Hostname))
	source = append(source, dataframe.NewDataFrameTag("service_name", datum.ServiceName))
	source = append(source, dataframe.NewDataFrameTag("metric", datum.Metric))
	source = append(source, dataframe.NewDataFrameTag("uom", unit.String()))
	return source
}

// Take a RenderedPerfDatumValue and return a DataFrame suitable for
// serialisation. This includes parsing the value of the metric (which
// is still a raw string in a RenderedPerfDatumValue) into its correct
// units.
func NewDataFrame(datum RenderedPerfDatumValue) (*dataframe.DataFrame, error) {
	frame := new(dataframe.DataFrame)
	// Need a variable to get an address
	frameTime := time.Unix(int64(datum.Timestamp), 0)
	timestamp := uint64(frameTime.UnixNano())
	rawValue, suffix := splitValueUOM(datum.Value)
	uom, err := ParseUOM(suffix)
	if err != nil {
		return nil, err
	}
	dataSource := getDataSource(datum, uom)
	switch {
	case isMicrosecondUOM(uom):
		value, err := parseMicroseconds(rawValue, uom)
		if err != nil {
			return nil, err
		}
		payload := dataframe.DataFrame_REAL
		frame.Payload = &payload
		frame.ValueMeasurement = &value
	case isByteUOM(uom):
		value, err := parseBytes(rawValue, uom)
		if err != nil {
			return nil, err
		}
		payload := dataframe.DataFrame_NUMBER
		frame.Payload = &payload
		frame.ValueNumeric = &value
	case uom == Counter:
		payload := dataframe.DataFrame_EMPTY
		frame.Payload = &payload
	// If we're given an empty string as UOM (slackers), we assume
	// it's an unadorned float and parse it accordingly. If that
	// fails, we treat it as empty/'counter'.
	case uom == NullUnit:
		value, err := strconv.ParseFloat(rawValue, 64)
		if err != nil {
			payload := dataframe.DataFrame_EMPTY
			frame.Payload = &payload
		} else {
			payload := dataframe.DataFrame_REAL
			frame.Payload = &payload
			frame.ValueMeasurement = &value
		}
	}
	frame.Source = dataSource
	frame.Timestamp = &timestamp
	return frame, nil
}
