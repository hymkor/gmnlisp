;;; The smake-template for release new version with Go 1.20.14
;;; It requires smake, jj or git, gh, and smake-go120.lsp
;;; Create an instance:
;;;     (defglobal bump (load "smake-bump.lsp"))
;;; Call a method:
;;;     (funcall bump)
(lambda ()
  (labels
    ((version-from-release-note
       (fname)
       (let ((version nil))
         (or
           (file-for-each
             fname
             (lambda (line)
               (if (setq version (match "^v([0-9]+)\.([0-9]+)\.([0-9]+)$" line))
                 (car version))))
           "v0.0.0")))

     (step-exec
       (command)
       (if (-d ".jj")
         (sh "jj log")
         (sh "git log"))
       (block b
         (while t
           (format (standard-output) "~&$ ~A~%[Y]es: execute, [N]o: skip, [Q]uit ? " command)
           (finish-output (standard-output))
           (case (read-line (standard-input))
             (("q") (return-from b nil))
             (("n") (return-from b t))
             (("y") (sh command) (return-from b t))))))
     )
    (let*
      ((j (-d ".jj"))
       (notes (wildcard "release_note*.md"))
       (version
         (if (consp notes)
           (progn
             (format (error-output) "Found: ~A~%" (car notes))
             (version-from-release-note (car notes)))
           "v0.0.0")))
      (and
        (step-exec
          (if j
            (string-append "jj commit -m \"bump to " version "\"")
            (string-append "git commit -m \"bump to " version "\" -a")))
        (step-exec (string-append "git tag " version))
        (if j
          (and (step-exec (string-append "jj bookmark set master -r " version))
               (step-exec "jj git push"))
          (step-exec "git push"))
        (step-exec "git push --tag")
        (step-exec (string-append *executable-name* " dist"))
        (step-exec (string-append *executable-name* " release"))
        (progn (sh "gh browse")
               (step-exec (string-append *executable-name* " manifest")))
        (if j
          (and (step-exec (string-append "jj commit -m \"Update the manifest of the scoop-installer for " version "\""))
               (step-exec "jj bookmark set master -r @-")
               (step-exec "jj git push"))
          (and (progn (sh "git status")
                      (step-exec (string-append "git commit -a -m \"Update the manifest of the scoop-installer for " version "\"")))
               (step-exec "git push"))))
      ) ; let
    ) ; labels
  )
