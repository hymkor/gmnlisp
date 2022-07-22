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
	go fmt cmd/preprocessor/main.go
	go build -o gmnlisp$(EXE) cmd/gmnlisp/main.go
	go build -o preprocessor$(EXE) cmd/preprocessor/main.go

test:
	go test -v

readme:
	.$(D)preprocessor$(EXE) _README.md > README.md
