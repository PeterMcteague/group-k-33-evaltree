 (ns group-k-33-evaltree)

;;Basic recursive solution
(defn evaltree
  ;;For turning from a list into the format we want
  ([list]
   (evaltree (first list) (first (rest list)) (first (rest (rest list))))
    )
  ;;For processing (expression num num) lists
  ([exp one two]
   (cond
     ;;If both inputs are numbers, then apply the expression to one and two, and create a new list with the result replacing the expression.
     (and (number? one) (number? two))   (cons (apply (resolve(symbol exp)) (cons one (cons two '())))    (cons one (cons two '())))
     ;;If the inputs are lists, then apply the expression to the solutions of both lists, and create a new list with the result replacing the expression.
     (and (list? one) (list? two))       (cons (apply (resolve(symbol exp)) (cons (first (evaltree one))  (cons (first (evaltree two)) ()))) (cons (evaltree one) (cons (evaltree two) '())))
     ;;If just one is a list , solve one and apply the expression to the result of one and the value of two, before creating a new list.
     (list? one)                         (cons (apply (resolve(symbol exp)) (cons (first (evaltree one))  (cons two '()))) (cons (evaltree one) (cons two '())))
     ;;If just two is a list , solve one and apply the expression to the result of two and the value of one, before creating a new list.
     (list? two)                         (cons (apply (resolve(symbol exp)) (cons one (cons (first (evaltree two)) '()))) (cons one (cons (evaltree two) '())))
     ;;else just return the list with the expression.
     :else                               (cons exp (cons one (cons two '())))
     )
    )
  )

;;A slightly better solution using nth instead of first and rest and using conj instead of multiple cons's.
(defn evaltree
  ;;For turning from a list into the format we want
  ([list]
   (evaltree (nth list 0) (nth list 1) (nth list 2))
    )
  ;;For processing (expression num num) lists
  ([exp one two]
   (cond
     ;;If both inputs are numbers, then apply the expression to one and two, and create a new list with the result replacing the expression.
     (and (number? one) (number? two))   (cons (apply (resolve(symbol exp)) (conj '() two one)) (conj '() two one))
     ;;If the inputs are lists, then apply the expression to the solutions of both lists, and create a new list with the result replacing the expression.
     (and (list? one) (list? two))       (cons (apply (resolve(symbol exp)) (conj '() (first (evaltree two)) (first (evaltree one)))) (conj '() (evaltree two) (evaltree one)))
     ;;If just one is a list , solve one and apply the expression to the result of one and the value of two, before creating a new list.
     (list? one)                         (cons (apply (resolve(symbol exp)) (conj '() two (first (evaltree one)))) (conj '() two (evaltree one)))
     ;;If just two is a list , solve one and apply the expression to the result of two and the value of one, before creating a new list.
     (list? two)                         (cons (apply (resolve(symbol exp)) (conj '() (first (evaltree two)) one)) (conj '() (evaltree two) one))
     ;;else just return the list with the expression.
     :else                               (conj '() two one exp)
     )
    )
  )

;;Test data
(evaltree '(* (+ 5 (* 3 7)) (- 6 8)))