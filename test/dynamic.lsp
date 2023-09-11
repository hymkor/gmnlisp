(dynamic-let
  ((a 1) (b 2))

  (test (dynamic a) 1)
  (test (dynamic b) 2)

  (dynamic-let
    ((a 3) (b 4))

    (test (dynamic a) 3)
    (test (dynamic b) 4))

  (test (dynamic a) 1)
  (test (dynamic b) 2))
