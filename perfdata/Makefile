all: install

install: build check
	go install

build: deps
	go build

deps:
	go get

check: 
	go test

.PHONY : all
