;;; test for (case)
(assert-eq (case 2
        ((1) "a")
        ((2) "b")
        ((3) "c")
        )
      "b")

(assert-eq (case 4
        ((1 2) "A")
        ((3 4) "B")
        ((5 6) "C")
        )
      "B")

(assert-eq (case 7
        ((1 2) "A")
        ((3 4) "B")
        (t "C")
        )
      "C")
