(setf (property 'zeus 'dauter) 'atehana)
(assert-eq (property 'zeus 'dauter) 'atehana)

(setf (property 'zeus 'dauter) 'who)
(assert-eq (property 'zeus 'dauter) 'who)

(remove-property 'zeus 'dauter)
(assert-eq (property 'zeus 'dauter 'not-found) 'not-found)
(assert-eq (property 'zeus 'dauter) nil)
