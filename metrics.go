package main

import (
	"fmt"
)

type CollectorMetricData struct {
	metrics map[string]string
}

func NewCollectorMetricData() *CollectorMetricData {
	c := new(CollectorMetricData)
	c.metrics = make(map[string]string)
	return c
}

// AddMetric will add a (key,value) pair, overwriting the value if the
// key already exists.
func (c *CollectorMetricData) AddMetric(key, value string) {
	c.metrics[key] = value
}

// IsSet returns true if the given key already has a value, and false if
// otherwise.
func (c *CollectorMetricData) IsSet(key string) bool {
	if _, ok := c.metrics[key]; ok {
		return true
	}
	return false
}

// String returns a representation suitable for easy machine-parsing of
// logs.
func (c *CollectorMetricData) String() string {
	output := ""
	for key, value := range c.metrics {
		output += fmt.Sprintf("metric:%v=%v\n", key, value)
	}
	return output
}

func (c *CollectorMetricData) Metrics() []string {
	output := make([]string, 0)
	for key, value := range c.metrics {
		output = append(output, fmt.Sprintf("metric:%v=%v\n", key, value))
	}
	return output
}
