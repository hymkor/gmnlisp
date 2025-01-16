; go run github.com/hymkor/smake@latest

(defun tail (path)
  (let ((buf (create-string-output-stream)))
    (format buf "tail \"~A\"" path)
    (sh (get-output-stream-string buf))))

(case (and *args* (car *args*))
  (("test" nil)
   (if (probe-file "errr")
     (rm "errr"))
   (if (probe-file "err")
     (progn
       (format (standard-output) "**** Previous log ****~%")
       (tail "err")
       (mv "err" "errr")))
   (let* ((curdir (getwd))
          (buf (create-string-output-stream))
          (err-path (joinpath curdir "err")))
     (pushd
       "__verify/tp-ipa"
       (format buf "\"~A\" -e ~S > \"~A\" 2>&1"
               (joinpath curdir "gmnlisp")
               "(load \"tp.lsp\") (tp-all 'verbose)"
               err-path)
       (sh (get-output-stream-string buf))
       (format (standard-output) "**** Latest log ****~%")
       (tail err-path)
       ); pushd
     ); let
   ); "test"
  (t
    (format (error-output) "smake ~S: not suport~%" (car *args*))
    )
  ); case

