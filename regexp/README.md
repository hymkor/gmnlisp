Regular Expression
==================

```
import (
    _ "github.com/hymkor/gmnlisp/regexp"
)
```

is required.

- (=~ REGEXP STRING)

compatible with "regexp".Regexp.FindAllStringSubmatch

``` lisp
(let ((m (=~ "a(x*)b" "-axxb-ab-")))
  (format t "ALL=~s~%" m)
  (format t "0,0=~s~%" (elt m 0 0))
  (format t "0,1=~s~%" (elt m 0 1))
  (format t "1,0=~s~%" (elt m 1 0))
  (format t "1,1=~s~%" (elt m 1 1))
  )
```

``` lisp
ALL=(("axxb" "xx") ("ab" ""))
0,0="axxb"
0,1="xx"
1,0="ab"
1,1=""
```

- (=~i REGEXP STRING)

compatible with "regexp".Regexp.FindAllStringSubmatchIndex

``` lisp
(let ((m (=~i "a(x*)b" "-axxb-ab-")))
  (format t "INDEXES=~s~%" m)
  )
```

``` lisp
INDEXES=((1 5 2 4) (6 8 7 7))
```
