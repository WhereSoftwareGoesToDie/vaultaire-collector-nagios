bletchley_import_perfdata: install

install: build check
	go install

build: deps
	go build

deps:
	go get

clean:
	rm -f bletchley_import_perfdata

check:
	go test	
