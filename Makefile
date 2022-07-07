all:
	go build && cd cmd/gmnlisp && go build -o ../../gmnlisp$(shell go env GOEXE)
