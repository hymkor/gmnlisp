(let ((|a b| "ahaha"))
  (test |a b| "ahaha")
  (test (convert '|a b| <string>) "a b"))

(let ((|ab| "ahaha"))
  (test ab "ahaha"))

(let ((|a\|b| "ihihi"))
  (test |a\|b| "ihihi")
  (test (convert '|a\|b| <string>) "a|b"))

(let ((|\\\\\|\\\|| 3))
  (test (convert '|\\\\\|\\\|| <string>) "\\\\|\\|"))
