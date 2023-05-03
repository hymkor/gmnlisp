;;; test for (case)
(test (case 2
        ((1) "a")
        ((2) "b")
        ((3) "c")
        )
      "b")

(test (case 4
        ((1 2) "A")
        ((3 4) "B")
        ((5 6) "C")
        )
      "B")

(test (case 7
        ((1 2) "A")
        ((3 4) "B")
        (t "C")
        )
      "C")
