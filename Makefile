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

$(NAME)$(EXE): $(wildcard *.go)
	go fmt
	go build
	go fmt cmd/gmnlisp/main.go
	cd cmd/gmnlisp && go build -o ../../gmnlisp$(EXE) -ldflags "-s -w -X main.version=$(VERSION)"

generate: embed.go sort-world newtypes.go stringer.go

all: $(NAME)$(EXE) gmnlpp$(EXE)

test:
	.$(D)gmnlisp test.lsp
	go fmt
	go test
	cd pkg/common && go test
	cd pkg/auto && go test

readme.md: _readme.md tools$(D)prepro.lsp
	.$(D)gmnlisp tools$(D)prepro.lsp < _README.md > README.md

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

embed.go: tools$(D)lsp2go.lsp embed.lsp
	.$(D)gmnlisp tools$(D)lsp2go.lsp gmnlisp < embed.lsp > embed.go

newtypes.go : newtypes.lsp Makefile
	gmnlisp $< gmnlisp "*StringBuilder" "*inputStream" "*_OutputFileStream" "*_Macro" "_ReaderNode" "_WriterNode" > $@

stringer.go : stringer.lsp Makefile
	gmnlisp $< gmnlisp ErrorNode Float Integer _WriterNode _ReaderNode _Macro _OutputFileStream inputStream _JoinedForm LispString SpecialF _Lambda _TrueType Cons Keyword Rune _NullType Array Function _Hash > $@

_package:
	$(SET) "CGO_ENABLED=0" && $(MAKE) clean && $(MAKE) all
	zip -9 $(NAME)-$(VERSION)-$(GOOS)-$(GOARCH).zip $(NAME)$(EXE) gmnlpp$(EXE)

package:
	$(SET) "GOOS=linux" && $(SET) "GOARCH=386"   && $(MAKE) _package
	$(SET) "GOOS=linux" && $(SET) "GOARCH=amd64" && $(MAKE) _package
	$(SET) "GOOS=windows" && $(SET) "GOARCH=386"   && $(MAKE) _package
	$(SET) "GOOS=windows" && $(SET) "GOARCH=amd64" && $(MAKE) _package
