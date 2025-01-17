(setf (property 'zeus 'dauter) 'atehana)
(assert-eq (property 'zeus 'dauter) 'atehana)

(setf (property 'zeus 'dauter) 'who)
(assert-eq (property 'zeus 'dauter) 'who)

(remove-property 'zeus 'dauter)
(assert-eq
  (catch 'ok
   (with-handler 
    (lambda (e) (throw 'ok "OK"))
    (format t "(6) ~S~%" (property 'zeus 'dauter))
    "NG")) "OK")
