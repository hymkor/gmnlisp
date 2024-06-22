(setf (property 'zeus 'dauter) 'atehana)
(test (property 'zeus 'dauter) 'atehana)

(setf (property 'zeus 'dauter) 'who)
(test (property 'zeus 'dauter) 'who)

(remove-property 'zeus 'dauter)
(test
  (catch 'ok
   (with-handler 
    (lambda (e) (throw 'ok "OK"))
    (format t "(6) ~S~%" (property 'zeus 'dauter))
    "NG")) "OK")
