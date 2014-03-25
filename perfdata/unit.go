package perfdata

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

const (
	_          = iota
	Kilo int64 = 1 << (10 * iota)
	Mega
	Giga
	Tera
)

// UOM represents a Nagios-format datatype. These are enumerated
// here[0]. Given the small range of possible types, we're just handling
// them individually.
//
// [0] https://www.monitoring-plugins.org/doc/guidelines.html#AEN200
type UOM int

const (
	NullUnit UOM = iota
	Microseconds
	Milliseconds
	Seconds
	Percentage
	Bytes
	Kilobytes
	Megabytes
	Gigabytes
	Terabytes
	Counter
)

// String returns the string representation of a UOM. This is not
// necessarily the same representation as used by Nagios or was present
// in the original perfdata.
func (u UOM) String() string {
	switch u {
	case NullUnit:
		return "NullUnit"
	case Microseconds:
		return "us"
	case Milliseconds:
		return "ms"
	case Percentage:
		return "%"
	case Bytes:
		return "B"
	case Kilobytes:
		return "KB"
	case Megabytes:
		return "MB"
	case Gigabytes:
		return "GB"
	case Terabytes:
		return "TB"
	case Counter:
		return "c"
	}
	return "Invalid"
}

// ParseUOM takes a Nagios-format unit suffix and returns a
// UOM value, plus an error on invalid suffix.
func ParseUOM(suffix string) (UOM, error) {
	switch strings.ToLower(suffix) {
	case "":
		return NullUnit, nil
	case "us":
		return Microseconds, nil
	case "ms":
		return Milliseconds, nil
	case "s":
		return Seconds, nil
	case "%":
		return Percentage, nil
	case "b":
		return Bytes, nil
	case "kb":
		return Kilobytes, nil
	case "mb":
		return Megabytes, nil
	case "gb":
		return Gigabytes, nil
	case "tb":
		return Terabytes, nil
	case "c":
		return Counter, nil
	}
	return NullUnit, fmt.Errorf("Invalid UOM suffix %s", suffix)
}

// isMicrosecondUOM returns true if unit is part of the set we're
// treating as microseconds (time values), otherwise false.
func isMicrosecondUOM(unit UOM) bool {
	switch unit {
	case Seconds, Milliseconds, Microseconds:
		return true
	}
	return false
}

// isByteUOM returns true if unit represents a byte count, otherwise
// false.
func isByteUOM(unit UOM) bool {
	switch unit {
	case Bytes, Kilobytes, Megabytes, Gigabytes, Terabytes:
		return true
	}
	return false
}

// isEmptyUOM returns true if unit is a UOM that isn't necessarily
// attached to a value (either a counter or unknown/invalid), otherwise
// false.
func isEmptyUOM(unit UOM) bool {
	switch unit {
	case NullUnit, Counter:
		return true
	}
	return false
}

// parseMicroseconds takes a time value and UOM and returns the number
// of microseconds it represents. Error on invalid string or invalid
// UOM.
func parseMicroseconds(data string, unit UOM) (float64, error) {
	value, err := strconv.ParseFloat(data, 64)
	if err != nil {
		return value, err
	}
	switch unit {
	case Seconds:
		return (value * float64(Mega)), nil
	case Milliseconds:
		return (value * float64(Kilo)), nil
	case Microseconds:
		return value, nil
	}
	return value, fmt.Errorf("Not a valid time UOM: %v", unit)
}

// parseBytes takes a byte value (MB, GB, et cetera) and UOM and returns
// the value in bytes. Error on invalid string or invalid UOM.
func parseBytes(data string, unit UOM) (int64, error) {
	value, err := strconv.ParseInt(data, 10, 64)
	if err != nil {
		return value, err
	}
	switch unit {
	case Bytes:
		return value, nil
	case Kilobytes:
		return (value * Kilo), nil
	case Megabytes:
		return (value * Mega), nil
	case Gigabytes:
		return (value * Giga), nil
	case Terabytes:
		return (value * Tera), nil
	}
	return value, fmt.Errorf("Not a valid byte UOM: %v", unit)
}

// splitValueUOM takes a raw value from Nagios perfdata and splits it
// into 'value' and 'unit' components (respectively).
//
// If the unit is missing, it returns the value and an empty string.
//
// If the expression can't be parsed at all (as a numeric value possibly
// followed by something), it returns two empty strings.
func splitValueUOM(data string) (string, string) {
	rxp := regexp.MustCompile("([0-9.-]+)([a-zA-Z]*)")
	matches := rxp.FindStringSubmatch(data)
	switch {
	// If there's no numeric data here at all, just return two empty
	// strings. There's a case for making this an error condition,
	// but there may be a legitimate use for it so we'll leave it
	// for now.
	//
	// The latter predicate in this expression Can't Happen.
	case matches == nil || len(matches) == 1:
		return "", ""
	// If there's no UOM specified, we return it as an empty string.
	case len(matches) == 2:
		return matches[1], ""
	}
	return matches[1], matches[2]
}
