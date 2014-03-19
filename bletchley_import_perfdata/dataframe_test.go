package main

import (
	"github.com/anchor/bletchley/dataframe"
	"github.com/anchor/bletchley/perfdata"
	"io/ioutil"
	"os"
	"testing"
)

func TestDataFrame(t *testing.T) {
	file := OpenTestPerfdataFile(t)
	perfLines := perfdata.ReadPerfDataRecords(file)
	renderedData := RenderTestPerfdata(t, perfLines)
	frame, err := perfdata.NewDataFrame(renderedData[3][1])
	if err != nil {
		t.Errorf("Failed to convert perfdatum %v into DataFrame: %v", renderedData[3][1], err)
	}
	fi, err := os.Open("testdata/test-perfdata-burst.pb")
	bytes, err := ioutil.ReadAll(fi)
	if err != nil {
		t.Errorf("Failed to read DataBurst test file: %v", err)
	}
	sampleBurst, err := dataframe.UnmarshalDataBurst(bytes)
	if err != nil {
		t.Errorf("Failed to unmarshal test burst: %v", err)
	}
	sampleFrame := sampleBurst.Frames[7]
	if !sampleFrame.Identical(*frame) {
		t.Errorf("Frame mismatch: expected %v, got %v", *sampleFrame, *frame)
	}
}
