package main

import (
	"fmt"
	"github.com/anchor/bletchley/dataframe"
	"github.com/anchor/bletchley/perfdata"
	"os"
)

// runBurstMode is intended for testing and/or generation of test data.
// It's not particularly robust, and all it does is read perfdata from
// stdin and write it in one DataBurst to stdout (possibly exhausting
// all available memory if supplied with sufficient perfdata). Do not
// run it in production or you will catch fire.
//
// Returns error only on catastrophic failure, merely complains and
// continues on failure to read or parse individual data.
func runBurstMode() error {
	frames := make([]*dataframe.DataFrame, 0)
	lines := perfdata.ReadPerfDataRecords(os.Stdin)
	for _, line := range lines {
		datum, err := perfdata.NewPerfDatum(line)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			continue
		}
		metrics, err := datum.RenderMetrics()
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			continue
		}
		for _, metricRecord := range metrics {
			frame, err := perfdata.NewDataFrame(metricRecord)
			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				continue
			}
			frames = append(frames, frame)
		}
	}
	burst := dataframe.BuildDataBurst(frames)
	rendered, err := dataframe.MarshalDataBurst(burst)
	if err != nil {
		return err
	}
	os.Stdout.Write(rendered)
	return nil
}
