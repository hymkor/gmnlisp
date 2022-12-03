;;; test for case-using
(assert
  (case-using
    #'= (+ 1.0 1.0)
    ((1) 'one)
    ((2 3) 'two-or-three)
    (t 'misc))
  'two-or-three)
(assert
  (case-using
    #'= (+ 1.0 2.0)
    ((1) 'one)
    ((2 3) 'two-or-three)
    (t 'misc))
  'two-or-three)
(assert
  (case-using
    #'= (+ 1.0 0.0)
    ((1) 'one)
    ((2 3) 'two-or-three)
    (t 'misc))
  'one)
