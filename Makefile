EXE=$(shell go env GOEXE)

ifeq ($(OS),Windows_NT)
    SHELL=CMD.EXE
    D=$\\
else
    D=/
endif


all:
	go fmt
	go build
	go fmt cmd/gmnlisp/main.go
	go build -o gmnlisp$(EXE) cmd/gmnlisp/main.go

preprocessor:
	go fmt
	go build
	go fmt cmd/gmnlpp/main.go
	go build -o gmnlpp$(EXE) cmd/gmnlpp/main.go

test:
	go test -v

readme: preprocessor
	.$(D)gmnlpp$(EXE) _README.md > README.md
