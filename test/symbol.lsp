(let ((|a b| "ahaha"))
  (assert-eq |a b| "ahaha")
  (assert-eq (convert '|a b| <string>) "a b"))

(let ((|ab| "ahaha"))
  (assert-eq ab "ahaha"))

(let ((|a\|b| "ihihi"))
  (assert-eq |a\|b| "ihihi")
  (assert-eq (convert '|a\|b| <string>) "a|b"))

(let ((|\\\\\|\\\|| 3))
  (assert-eq (convert '|\\\\\|\\\|| <string>) "\\\\|\\|"))
