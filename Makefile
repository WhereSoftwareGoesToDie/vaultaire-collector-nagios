vaultaire-collector-nagios: install

install: build check
	go install

build: deps
	go build

deps:
	go get

clean:
	rm -f vaultaire-collector-nagios

check:
	go test	
