package gmnlisp

var stringBuilderClass = registerNewBuiltInClass[*StringBuilder]("<string-builder>")

func (*StringBuilder) ClassOf() Class {
	return stringBuilderClass
}

func (t *StringBuilder) Equals(Node, EqlMode) bool {
	return false
}

var inputStreamClass = registerNewBuiltInClass[*inputStream]("<input-stream>")

func (t *inputStream) ClassOf() Class {
	return inputStreamClass
}

func (t *inputStream) Equals(Node, EqlMode) bool {
	return false
}

func (t *inputStream) String() string {
	return "(*inputStream)"
}

var outputFileStreamClass = registerNewBuiltInClass[*_OutputFileStream]("<output-file-stream>")

func (*_OutputFileStream) ClassOf() Class {
	return outputFileStreamClass
}

func (t *_OutputFileStream) Equals(Node, EqlMode) bool {
	return false
}

func (t *_OutputFileStream) String() string {
	return "(*_OutputFileStream)"
}

func (t *_Macro) Equals(Node, EqlMode) bool {
	return false
}

func (t *_Macro) String() string {
	return "(*_Macro)"
}

var readerNodeClass = registerNewBuiltInClass[_ReaderNode]("<reader>")

func (_ReaderNode) ClassOf() Class {
	return readerNodeClass
}

func (t _ReaderNode) Equals(Node, EqlMode) bool {
	return false
}

func (t _ReaderNode) String() string {
	return "(_ReaderNode)"
}

var writerNodeClass = registerNewBuiltInClass[_WriterNode]("<_WriterNode>")

func (_WriterNode) ClassOf() Class {
	return writerNodeClass
}

func (t _WriterNode) Equals(Node, EqlMode) bool {
	return false
}

func (t _WriterNode) String() string {
	return "(_WriterNode)"
}
