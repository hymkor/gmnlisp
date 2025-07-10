ifeq ($(OS),Windows_NT)
    SHELL=CMD.EXE
    SET=set
    CP=copy
    RM=del
    NUL=nul
    WHICH=where
else
    SET=export
    CP=cp
    RM=rm
    NUL=/dev/null
    WHICH=which
endif

ifndef GO
    SUPPORTGO=go1.20.14
    GO:=$(shell $(WHICH) $(SUPPORTGO) 2>$(NUL) || echo go)
endif

NAME:=$(notdir $(CURDIR))
EXE:=$(shell go env GOEXE)
VERSION:=$(shell git describe --tags || echo v0.0.0)

TARGET:=$(NAME)$(EXE)
RUNLISP?="./$(TARGET)"
GENERATES=sort-world

all:
	$(GO) fmt ./...
	$(GO) build
	$(GO) fmt cmd/gmnlisp/main.go
	$(GO) build -C cmd/gmnlisp -o "$(CURDIR)/$(TARGET)" -ldflags "-s -w -X main.version=$(VERSION)"

generate: $(GENERATES)

### test ###

test:
	$(RUNLISP) test.lsp
	$(GO) fmt
	$(GO) test

### Updating documents

README.md: tools/prepro.lsp _readme.md
	$(RUNLISP) $< < _README.md > $@

### Cleaning ###

clean:
	$(RM) $(TARGET)

clean-gen:
	$(RM) $(GENERATES)

### Formating sources ###

sort-world:
	$(RUNLISP) tools/gosort.lsp < world.go > world.go_
	-cmp world.go world.go_ || $(CP) world.go_ world.go
	$(RM) world.go_

### Packaging ###

_dist:
	$(SET) "CGO_ENABLED=0" && $(MAKE) clean && $(MAKE) all
	zip -9 $(NAME)-$(VERSION)-$(GOOS)-$(GOARCH).zip $(TARGET)

dist:
	$(SET) "GOOS=linux" && $(SET) "GOARCH=386"   && $(MAKE) _dist
	$(SET) "GOOS=linux" && $(SET) "GOARCH=amd64" && $(MAKE) _dist
	$(SET) "GOOS=windows" && $(SET) "GOARCH=386"   && $(MAKE) _dist
	$(SET) "GOOS=windows" && $(SET) "GOARCH=amd64" && $(MAKE) _dist

manifest:
	make-scoop-manifest *-windows-*.zip > $(NAME).json

release:
	gh release create -d --notes "" -t $(VERSION) $(VERSION) $(wildcard $(NAME)-$(VERSION)-*.zip)

download-verify:
	mkdir "$(CURDIR)/__verify" && \
	cd __verify && \
	curl -O http://islisp.org/program/Verify.zip && \
	unzip Verify.zip && \
	unzip tp-ipa.zip

verify:
	cd "$(CURDIR)/__verify/tp-ipa" && \
	"$(CURDIR)/gmnlisp" -strict "$(CURDIR)/verify.lsp" "$(TEMP)" 2>&1

verify-verbose:
	cd "$(CURDIR)/__verify/tp-ipa" && \
	"$(CURDIR)/gmnlisp" -strict "$(CURDIR)/verify.lsp" "$(TEMP)" verbose 2>&1

.PHONY: generate test clean clean-gen sort-world _dist dist manifest
