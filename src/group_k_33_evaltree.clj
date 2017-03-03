 (ns group-k-33-evaltree)

;;Basic recursive solution
(defn evaltree-recursive
  ;;If we get a valid list, make a recursive call with the parts of the list as the arguments.
  ([list]
    (if (= 3 (count list))
      (evaltree-recursive (first list) (first (rest list)) (first (rest (rest list))))
      "The list entered is invalid"
      ))
  ;;If we've got in an (exponent argument1 argument2) list
  ([exp one two]
   (cond
     ;;If both inputs are numbers, then apply the expression to one and two, and create a new list with the result replacing the expression.
     (and (number? one) (number? two))
          (cons (apply (resolve(symbol exp)) (cons one (cons two '())))    (cons one (cons two '())))
     ;;If the inputs are lists, then apply the expression to the solutions of both lists, and create a new list with the result replacing the expression.
     (and (and (list? one) (list? two))  (and (= 3 (count one)) (= 3 (count two))))
          (cons (apply (resolve(symbol exp)) (cons (first (evaltree-recursive one))  (cons (first (evaltree-recursive two)) ()))) (cons (evaltree-recursive one) (cons (evaltree-recursive two) '())))
     ;;If just one is a list , solve one and apply the expression to the result of one and the value of two, before creating a new list.
     (and (list? one) (= 3 (count one)))
          (cons (apply (resolve(symbol exp)) (cons (first (evaltree-recursive one))  (cons two '()))) (cons (evaltree-recursive one) (cons two '())))
     ;;If just two is a list , solve one and apply the expression to the result of two and the value of one, before creating a new list.
     (and (list? two) (= 3 (count two)))
          (cons (apply (resolve(symbol exp)) (cons one (cons (first (evaltree-recursive two)) '()))) (cons one (cons (evaltree-recursive two) '())))
     ;;else just return the list with the expression.
     :else
          (cons exp (cons one (cons two '())))
     )
    )
  )

;;A slightly better solution using nth instead of first and rest and using conj instead of multiple cons's.
 (defn evaltree-recursive-v2
   ;;If we get a valid list, make a recursive call with the parts of the list as the arguments.
   ([list]
     (if (= 3 (count list))
       (evaltree-recursive-v2 (nth list 0) (nth list 1) (nth list 2))
        "The list entered is invalid"
       ))
   ;;If we've got in an (exponent argument1 argument2) list
   ([exp one two]
     (cond
       ;;If both inputs are numbers, then apply the expression to one and two, and create a new list with the result replacing the expression.
       (and (number? one) (number? two))
            (cons (apply (resolve(symbol exp)) (conj '() two one)) (conj '() two one))
       ;;If the inputs are lists, then apply the expression to the solutions of both lists, and create a new list with the result replacing the expression.
       (and (and (list? one) (list? two))  (and (= 3 (count one)) (= 3 (count two))))
            (cons (apply (resolve(symbol exp)) (conj '() (first (evaltree-recursive-v2 two)) (first (evaltree-recursive-v2 one)))) (conj '() (evaltree-recursive-v2 two) (evaltree-recursive-v2 one)))
       ;;If just one is a list , solve one and apply the expression to the result of one and the value of two, before creating a new list.
       (and (list? one) (= 3 (count one)))
            (cons (apply (resolve(symbol exp)) (conj '() two (first (evaltree-recursive-v2 one)))) (conj '() two (evaltree-recursive-v2 one)))
       ;;If just two is a list , solve one and apply the expression to the result of two and the value of one, before creating a new list.
       (and (list? two) (= 3 (count two)))
            (cons (apply (resolve(symbol exp)) (conj '() (first (evaltree-recursive-v2 two)) one)) (conj '() (evaltree-recursive-v2 two) one))
       ;;else just return the list with the expression.
       :else
            (conj '() two one exp)
       )
     )
   )

 ;;Tests for first recursive solution
 (evaltree-recursive '(* 3 2))
 (evaltree-recursive '(* (+ 1 2) (+ -5 6)))
 (evaltree-recursive '(* 2 (+ -5 6)))
 (evaltree-recursive '(* (+ -5 6) 2))
 (evaltree-recursive '(* () 2))
 (evaltree-recursive '(* 2))
 (evaltree-recursive '(* (+ 5 (* 3 7)) (- 6 8)))
;;Tests for v2 recursive solution
 (evaltree-recursive-v2 '(* 3 2))
 (evaltree-recursive-v2 '(* (+ 1 2) (+ -5 6)))
 (evaltree-recursive-v2 '(* 2 (+ -5 6)))
 (evaltree-recursive-v2 '(* (+ -5 6) 2))
 (evaltree-recursive-v2 '(* () 2))
 (evaltree-recursive-v2 '(* 2))
 (evaltree-recursive-v2 '(* (+ 5 (* 3 7)) (- 6 8)))

 ;;Breaks if it's trying to solve a list containing an empty bracket.