package perfdata

import (
	"errors"
	"regexp"
	"strings"
)

// HostPerfDatum implements the PerfDatum interface and represents
// results from Nagios host checks.
type HostPerfDatum struct {
	Timestamp      string
	HostName       string
	PerfDataString string
	State          string
	StateType      string
	CheckCommand   string

	perfDataValues map[string]string
}

func NewHostPerfDatum(fields map[string]string) (*HostPerfDatum, error) {
	datum := new(HostPerfDatum)

	datum.Timestamp = fields["TIMET"]
	datum.HostName = fields["HOSTNAME"]
	datum.PerfDataString = fields["HOSTPERFDATA"]
	datum.CheckCommand = fields["HOSTCHECKCOMMAND"]
	datum.State = fields["HOSTSTATE"]
	datum.StateType = fields["HOSTSTATETYPE"]

	return datum, nil
}

func (d HostPerfDatum) PerfData() ([]PerfDatumValue, error) {
	rendered := make([]PerfDatumValue, 0)
	data := strings.Split(d.PerfDataString, " ")
	for _, datum := range data {
		rxp := regexp.MustCompile("([a-z]+)=([^; ]+);([^; ]*);([^; ]*);")
		matchSlice := rxp.FindStringSubmatch(datum)
		if len(matchSlice) < 4 {
			return rendered, errors.New("Could not parse perfdata string")
		}
		rendered = append(rendered, PerfDatumValue{matchSlice[1], matchSlice[2], matchSlice[3], matchSlice[4]})
	}
	return rendered, nil
}

func (d HostPerfDatum) GetServiceName() string {
	return "host"
}

func (d HostPerfDatum) GetHostName() string {
	return d.HostName
}

func (d HostPerfDatum) GetState() string {
	return d.State
}

func (d HostPerfDatum) GetTimestamp() string {
	return d.Timestamp
}

// Take all our PerfDatumValues and render them into a flat list of
// RenderedPerfDatumValues suitable for storage.
func (d HostPerfDatum) RenderMetrics() ([]RenderedPerfDatumValue, error) {
	data, err := d.PerfData()
	if err != nil {
		return nil, err
	}
	return RenderPerfMetrics(d, data), nil
}
