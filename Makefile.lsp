; go run github.com/hymkor/smake@latest {test or release}

(defglobal make (load "smake-go120.lsp"))

(case (and *args* (car *args*))
  (("verify")
   (with-error-output
     (standard-output)
     (pushd
       "__verify/tp-ipa"
       (spawn "../../gmnlisp"
              "-strict"
              "../../verify.lsp"
              (getenv "TEMP")))))

  (("test")
   (spawn "./gmnlisp" "test.lsp")
   (funcall make 'fmt)
   (funcall make 'test))

  (("bump")
   (let ((bump (load "smake-bump.lsp")))
     (funcall bump)))

  (("release" "manifest" "clean" "env")
   (funcall make $1))

  (("build" "" nil)
   (funcall make 'build)
   (funcall make 'build-cmd "cmd/gmnlisp"))

  (("dist")
   (mapc
     (lambda (platform)
       (env (("GOOS"   (car platform))
             ("GOARCH" (cdr platform)))
            (funcall make 'build)
            (let ((aout (funcall make 'build-cmd "cmd/gmnlisp")))
              (funcall make 'dist aout))
            ))
     (list (cons "linux" "386")
           (cons "linux" "amd64")
           (cons "windows" "386")
           (cons "windows" "amd64")))
   )

  (t
    (format (error-output) "Usage: smake {build|dist|release|manifest|bump|test}~%")
    ) ; t
  ) ; case

