all:
	go fmt
	go build
	go fmt cmd/gmnlisp/main.go
	go build -o gmnlisp$(shell go env GOEXE) cmd/gmnlisp/main.go

test:
	go test -v
