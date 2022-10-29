all:
	go fmt
	go build

embed.go : embed.lsp
	gmnlisp ../../lsp2go.lsp common < embed.lsp > embed.go

test :
	go test
