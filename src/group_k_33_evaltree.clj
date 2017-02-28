 (ns group-k-33-evaltree)

 (defn evaltree
   ;;For turning from a list into the format we want
   ([list]
    (evaltree (first list) (first (rest list)) (first (rest (rest list))))
     )
   ([exp one two]
     ;;Easy case, one and two are both numbers
    (cond
      ;;If both inputs are numbers, then calculate and return the result in space of the operation.
      (and (number? one) (number? two))   (cons (apply(resolve(symbol exp))(cons one (cons two '()))) (cons one (cons two '())))
      ;;If the inputs are lists
      (and (list? one) (list? two))       (cons (apply (resolve (symbol exp)) (cons (first (evaltree one))  (cons (first (evaltree two)) ()))) (cons (evaltree one) (cons (evaltree two) '())))
      ;;If just one is a list
      (list? one)                         (cons (apply (resolve (symbol exp)) (cons (first (evaltree one))  (cons two '()))) (cons (evaltree one) (cons two '())))
      ;;If just two is a list
      (list? two)                         (cons (apply (resolve (symbol exp)) (cons one (cons (first (evaltree two)) '()))) (cons one (cons (evaltree two) '())))
      ;;else just return with the symbol
      :else                               (cons exp (cons one (cons two '())))
      )
     )
   )

(evaltree '(* (+ 5 (* 3 7)) (- 6 8)))