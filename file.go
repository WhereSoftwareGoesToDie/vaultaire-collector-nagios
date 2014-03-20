package main

import (
	"github.com/anchor/bletchley/dataframe"
	"github.com/anchor/bletchley/framestore"
	"github.com/anchor/bletchley/perfdata"
	"os"
)

type BletchleyDataFrameFileWriter struct {
	stream   *os.File
	filename string
}

func NewBletchleyDataFrameFileWriter(filename string) (*BletchleyDataFrameFileWriter, error) {
	var err error
	writer := new(BletchleyDataFrameFileWriter)
	writer.filename = filename
	writer.stream, err = os.OpenFile(filename, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0644)
	if err != nil {
		framestore.Log.Errorf("Could not create file %v: %v", filename, err)
		return nil, err
	}
	return writer, nil
}

func (w BletchleyDataFrameFileWriter) Write(datum perfdata.RenderedPerfDatumValue) error {
	frame, err := perfdata.NewDataFrame(datum)
	if err != nil {
		return err
	}
	marshalledMessage, err := dataframe.MarshalDataFrame(frame)
	if err != nil {
		return err
	}
	_, err = w.stream.Write(marshalledMessage)
	return err
}

func (w BletchleyDataFrameFileWriter) Shutdown() error {
	w.stream.Close()
	return nil
}
