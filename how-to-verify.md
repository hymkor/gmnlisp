How to execute the ISLisp verification program
==============================================

$ **`make download-verify`**

```
mkdir "C:/Users/hymkor/src/gmnlisp/__verify" && \
cd __verify && \
curl -O http://islisp.org/program/Verify.zip && \
unzip Verify.zip && \
unzip tp-ipa.zip
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 76761  100 76761    0     0   210k      0 --:--:-- --:--:-- --:--:--  212k
Archive:  Verify.zip
  inflating: readme.txt
  inflating: tp-ipa.zip
Archive:  tp-ipa.zip
   creating: tp-ipa/
   creating: tp-ipa/data/
  inflating: tp-ipa/data/array.lsp
   creating: __MACOSX/
:
:
  inflating: __MACOSX/tp-ipa/._tp.lsp
```

$ **`make verify`**

```
cd "C:/Users/hymkor/src/gmnlisp/__verify/tp-ipa" && \
"C:/Users/hymkor/src/gmnlisp/gmnlisp" -e "(load \"tp.lsp\") (tp-all)"
> TP File : data/formeval.lsp
> NG: ((lambda (nil) nil) 1) -> nil [#<Error> <program-error>]
> NG: ((lambda (t) nil) 1) -> nil [#<Error> <program-error>]
:
:
> NG: (undefined-entity-namespace "a1.23410nil\x05") -> #<Correct type of argument #1> [#<Wrong type of argument #1>]
> TP File : data/misc.lsp
> TP Result: OK = 8214, NG = 8197
```
