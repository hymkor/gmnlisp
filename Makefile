NAME=$(lastword $(subst /, ,$(abspath .)))
EXE=$(shell go env GOEXE)
VERSION=$(shell git describe --tags)

ifeq ($(OS),Windows_NT)
    SHELL=CMD.EXE
    SET=set
    RM=del
    D=$\\
else
    SET=export
    RM=rm
    D=/
endif

$(NAME)$(EXE): $(wildcard *.go) embed.lsp embed.go
	go fmt
	go build
	go fmt cmd/gmnlisp/main.go
	cd cmd/gmnlisp && go build -o ../../gmnlisp$(EXE) -ldflags "-s -w -X main.version=$(VERSION)"

all: $(NAME)$(EXE) gmnlpp$(EXE)

test:
	go fmt
	go test
	cd pkg/common && go test
	cd pkg/auto && go test
	.$(D)gmnlisp test.lsp

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

embed.go: lsp2go.lsp embed.lsp
	gmnlisp lsp2go.lsp gmnlisp < embed.lsp > embed.go

_package:
	$(SET) "CGO_ENABLED=0" && $(MAKE) clean && $(MAKE) all
	zip -9 $(NAME)-$(VERSION)-$(GOOS)-$(GOARCH).zip $(NAME)$(EXE) gmnlpp$(EXE)

package:
	$(SET) "GOOS=linux" && $(SET) "GOARCH=386"   && $(MAKE) _package
	$(SET) "GOOS=linux" && $(SET) "GOARCH=amd64" && $(MAKE) _package
	$(SET) "GOOS=windows" && $(SET) "GOARCH=386"   && $(MAKE) _package
	$(SET) "GOOS=windows" && $(SET) "GOARCH=amd64" && $(MAKE) _package
