EXE=$(shell go env GOEXE)
VERSION=$(shell git describe --tags)

ifeq ($(OS),Windows_NT)
    SHELL=CMD.EXE
    RM=del
    D=$\\
else
    RM=rm
    D=/
endif


all:
	go fmt
	go build
	go fmt cmd/gmnlisp/main.go
	cd cmd/gmnlisp && go build -o ../../gmnlisp$(EXE) -ldflags "-s -w -X main.version=$(VERSION)"

test:
	go fmt
	go test
	cd pkg/common && go test
	cd pkg/auto && go test

readme.md: _readme.md gmnlpp$(EXE)
	.$(D)gmnlpp$(EXE) _README.md > README.md

gmnlpp$(EXE): gmnlisp$(EXE)
	go fmt
	go build
	go fmt cmd/gmnlpp/main.go
	go build -o gmnlpp$(EXE) cmd/gmnlpp/main.go

clean:
	$(RM) gmnlpp$(EXE) gmnlisp$(EXE)

sort-world:
	gmnlisp gosort.lsp < world.go > world.go_
	-cmp world.go world.go_ || copy world.go_ world.go
	del world.go_
