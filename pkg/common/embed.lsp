(defmacro set-nth (newvalue Z L)
  `(set-elt ,newvalue ,L ,Z))
