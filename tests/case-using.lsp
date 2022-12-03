;;; test for case-using
(test
  (case-using
    #'= (+ 1.0 1.0)
    ((1) 'one)
    ((2 3) 'two-or-three)
    (t 'misc))
  'two-or-three)
(test
  (case-using
    #'= (+ 1.0 2.0)
    ((1) 'one)
    ((2 3) 'two-or-three)
    (t 'misc))
  'two-or-three)
(test
  (case-using
    #'= (+ 1.0 0.0)
    ((1) 'one)
    ((2 3) 'two-or-three)
    (t 'misc))
  'one)
