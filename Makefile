all:
	go fmt
	go build
	go fmt cmd/gmnlisp/main.go
	go fmt cmd/preprocessor/main.go
	go build -o gmnlisp$(shell go env GOEXE) cmd/gmnlisp/main.go
	go build -o preprocessor$(shell go env GOEXE) cmd/preprocessor/main.go

test:
	go test -v

list:
	gmnlisp -e "(setq n (--get-all-symbols--)) (while n (write-line (car n)) (setq n (cdr n)))" | sort | uniq | gawk "{ printf \"- `%%s`\n\",$$0 }"
