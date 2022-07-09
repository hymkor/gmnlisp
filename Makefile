all:
	go fmt && go build && cd cmd/gmnlisp && go fmt && go build -o ../../gmnlisp$(shell go env GOEXE)
