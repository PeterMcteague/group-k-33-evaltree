 (ns group-k-33-evaltree)

 (defn evaltree
   ([list]
    (evaltree (first list) (first (rest list)) (first (rest (rest list))))
     )
   ([exp one two]
     ;;Easy case, one and two are both numbers
    (if (and (number? one) (number? two))
        (cons (apply (resolve (symbol exp)) (cons one (cons two ()))) (cons one (cons two '())))
      ))
   )