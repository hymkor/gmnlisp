all:
	go fmt
	go build
	go fmt cmd/gmnlisp/main.go
	go fmt cmd/preprocessor/main.go
	go build -o gmnlisp$(shell go env GOEXE) cmd/gmnlisp/main.go
	go build -o preprocessor$(shell go env GOEXE) cmd/preprocessor/main.go

test:
	go test -v

readme:
	./preprocessor.exe _README.md > README.md
