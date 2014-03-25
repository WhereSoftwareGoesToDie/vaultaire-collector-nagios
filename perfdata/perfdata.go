/*
The perfdata package contains tools for working with performance data
from Nagios and compatible monitoring systems (perfdata).
*/
package perfdata

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

// PerfDatumValue is one measurment of one metric (so you might have
// several per service-or-host check/PerfDatum).
//
// It differs from RenderedPerfDatumValue in that it's not a complete
// specification of the source of the metric (doesn't contain
// Hostname or Timestamp).
//
// This will probably be factored out in a future version.
type PerfDatumValue struct {
	Name  string
	value string
	min   string
	max   string
}

// RenderedPerfDatumValue is a flattened version of the above with
// metadata from the associated service check, representing a discrete
// data point ready to be shoved into a time-series store (after it's
// been munged a bit).
type RenderedPerfDatumValue struct {
	Metric      string `json:"metric"`
	Value       string `json:"value"`
	Timestamp   uint64 `json:"timestamp"`
	ServiceName string `json:"service_name"`
	Hostname    string `json:"hostname"`
}

// GetKey returns a unique identifier for a RenderedPerfDatumValue in
// string format, as underscore-separated values hostname, service name,
// metric name and timestamp.
func (d RenderedPerfDatumValue) GetKey() string {
	key := fmt.Sprintf("%v_%v_%v_%v", d.Hostname, d.ServiceName, d.Metric, d.Timestamp)
	return key
}

// PerfDatum is an interface type representing one Nagios check's
// perfdata (so one line in the perfdata file). It's implemented by
// ServicePerfDatum and HostPerfDatum.
type PerfDatum interface {
	PerfData() ([]PerfDatumValue, error)

	GetHostName() string
	GetState() string
	GetTimestamp() string
	// This should return "host" if it's a host check, and otherwise return
	// the service name.
	GetServiceName() string

	RenderMetrics() ([]RenderedPerfDatumValue, error)
}

// NewPerfDatum takes a slice of perfdata fields from one record, i.e.,
// the slice::value bits from a single line (which might contain many
// individual metrics, if the Nagios check reports more than one piece
// of perfdata per check).
//
// Return value is a PerfDatum, which is an interface type implemented
// by HostPerfDatum and ServicePerfDatum.
//
// A PerfDatum can be of either the 'host' or 'service' types, which
// must be parsed slightly differently; this is specified in the
// 'DATATYPE' field, i.e., 'DATATYPE::SERVICEPERFDATA' or
// 'DATATYPE::HOSTPERFDATA'.
//
// FIXME: do we need to normalize the case here, or is it guaranteed?
func NewPerfDatum(record []string) (PerfDatum, error) {
	datum := make(map[string]string, 0)
	for _, field := range record {
		parts := strings.Split(field, "::")
		// Something's fucked, bail out.
		if len(parts) < 2 {
			return nil, errors.New("Couldn't split perfdata field into key and value.")
		}
		// FIXME: sanity check field names/count
		datum[parts[0]] = parts[1]
	}
	if datum["DATATYPE"] == "HOSTPERFDATA" {
		return NewHostPerfDatum(datum)
	} else if datum["DATATYPE"] == "SERVICEPERFDATA" {
		return NewServicePerfDatum(datum)
	}
	return nil, errors.New("DATATYPE must be SERVICEPERFDATA or HOSTPERFDATA")
}

// RenderPerfMetrics takes a PerfDatum (representing a service check)
// and a slice of PerfDatumValues (representing individual metrics
// obtained from that service check). It flattens these into a slice of
// RenderedPerfDatumValues, which are flattened versions of both inputs
// (individual metric data and necessary service-check metadata).
func RenderPerfMetrics(pd PerfDatum, data []PerfDatumValue) []RenderedPerfDatumValue {
	// We want to include the Nagios service's state as a metric
	// rather than tagging every RenderedPerfDatumValue.
	serviceState := new(PerfDatumValue)
	serviceState.Name = "service_state"
	serviceState.value = pd.GetState()
	data = append(data, *serviceState)

	metrics := make([]RenderedPerfDatumValue, 0)
	for _, datum := range data {
		metric := new(RenderedPerfDatumValue)
		metric.Metric = datum.Name
		metric.Value = datum.value
		intTimestamp, _ := strconv.ParseUint(pd.GetTimestamp(), 10, 64)
		metric.Timestamp = intTimestamp
		metric.ServiceName = pd.GetServiceName()
		metric.Hostname = pd.GetHostName()
		metrics = append(metrics, *metric)
	}
	return metrics
}
