package main

import (
	"github.com/anchor/bletchley/framestore/vaultaire"
	"github.com/anchor/vaultaire-collector-nagios/perfdata"
)

type BletchleyDataFrameVaultaireWriter struct {
	writer vaultaire.VaultaireWriter
}

func NewBletchleyDataFrameVaultaireWriter(broker string, batchPeriod float64, origin, telemetry string, debug bool) (*BletchleyDataFrameVaultaireWriter, error) {
	var err error
	writer := new(BletchleyDataFrameVaultaireWriter)
	writer.writer, err = vaultaire.NewVaultaireWriter(broker, batchPeriod, origin, telemetry, debug)
	if err != nil {
		Log.Errorf("Could not initialize connection to chateau at %v: %v", broker, err)
		return nil, err
	}
	return writer, nil
}

func (w BletchleyDataFrameVaultaireWriter) Write(datum perfdata.RenderedPerfDatumValue) error {
	frame, err := perfdata.NewDataFrame(datum)
	if err != nil {
		return err
	}
	return w.writer.WriteFrame(frame)
}

func (w BletchleyDataFrameVaultaireWriter) Shutdown() error {
	return w.writer.Shutdown()
}
