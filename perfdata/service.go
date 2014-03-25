package perfdata

import (
	"errors"
	"regexp"
	"strings"
)

// ServicePerfDatum implements the PerfDatum interface and represents
// results from Nagios service checks.
type ServicePerfDatum struct {
	Timestamp          string
	HostName           string
	ServiceDescription string
	HostState          string
	HostStateType      string
	PerfDataString     string
	CheckCommand       string
	State              string
	StateType          string

	perfDataValues map[string]string
}

// NewServicePerfDatum builds a ServicePerfDatum from the list of fields
// we got from splitting the perfdata output.
func NewServicePerfDatum(fields map[string]string) (*ServicePerfDatum, error) {
	datum := new(ServicePerfDatum)

	datum.Timestamp = fields["TIMET"]
	datum.HostName = fields["HOSTNAME"]
	datum.ServiceDescription = fields["SERVICEDESC"]
	datum.PerfDataString = fields["SERVICEPERFDATA"]
	datum.CheckCommand = fields["SERVICECHECKCOMMAND"]
	datum.State = fields["SERVICESTATE"]
	datum.StateType = fields["SERVICESTATETYPE"]
	datum.HostState = fields["HOSTSTATE"]
	datum.HostStateType = fields["HOSTSTATETYPE"]

	return datum, nil
}

// PerfData extracts the individual metrics/values from a ServicePerfDatum.
func (d ServicePerfDatum) PerfData() ([]PerfDatumValue, error) {
	rendered := make([]PerfDatumValue, 0)
	data := strings.Split(d.PerfDataString, " ")
	for _, datum := range data {
		datum = strings.Trim(datum, " \t\n")
		if datum == "" {
			continue
		}
		rxp := regexp.MustCompile("([^=;]+)=(([^; ]+);([^; ]*);([^; ]*);?)?")
		matchSlice := rxp.FindStringSubmatch(datum)
		if len(matchSlice) < 4 {
			return rendered, errors.New(datum)
		}
		rendered = append(rendered, PerfDatumValue{matchSlice[1], matchSlice[3], matchSlice[4], matchSlice[5]})
	}
	return rendered, nil
}

func (d ServicePerfDatum) GetServiceName() string {
	return d.ServiceDescription
}

func (d ServicePerfDatum) GetHostName() string {
	return d.HostName
}

func (d ServicePerfDatum) GetState() string {
	return d.State
}

func (d ServicePerfDatum) GetTimestamp() string {
	return d.Timestamp
}

// Take all our PerfDatumValues and render them into a flat list of
// RenderedPerfDatumValues suitable for storage.
func (d ServicePerfDatum) RenderMetrics() ([]RenderedPerfDatumValue, error) {
	data, err := d.PerfData()
	if err != nil {
		return nil, err
	}
	return RenderPerfMetrics(d, data), nil
}
