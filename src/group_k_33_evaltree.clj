 (ns group-k-33-evaltree)

;;A function to perform similarly to say whether two things are the same
 (defn assertequals [expected answer]
   (println "Expected: " expected)
   (println "Answer: " answer)
   (= expected answer)
   )

;;A function for finding if a list contains an item (contains? was deprecated in an earlier version).
;;Taken from http://stackoverflow.com/a/3249777
(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

;;Basic recursive solution
(defn evaltree-recursive
  ;;If we get a valid list, make a recursive call with the parts of the list as the arguments.
  ([list]
   (if (and (= 3 (count list)) (not (in? list '())) )
      (evaltree-recursive (first list) (first (rest list)) (first (rest (rest list))))
      list
      ))
  ;;If we've got in an (exponent argument1 argument2) list
  ([exp one two]
   (cond
     ;;If both inputs are numbers, then apply the expression to one and two, and create a new list with the result replacing the expression.
     (and (number? one) (number? two))
          (cons
            (apply (resolve(symbol exp)) (cons one (cons two '())))
            (cons one (cons two '())))

     ;;If the inputs are lists, then apply the expression to the solutions of both lists, and create a new list with the result replacing the expression.
     (and (and (list? one) (list? two)) (and (not= one (evaltree-recursive one)) (not= two (evaltree-recursive two))))
          (cons
            (apply (resolve(symbol exp)) (cons (first (evaltree-recursive one))  (cons (first (evaltree-recursive two)) ())))
            (cons (evaltree-recursive one) (cons (evaltree-recursive two) '())))

     ;;If just one is a list , solve one and apply the expression to the result of one and the value of two, before creating a new list.
     (and (number? two) (and (list? one) (not= one (evaltree-recursive one))))
          (cons
            (apply (resolve(symbol exp)) (cons (first (evaltree-recursive one))  (cons two '())))
            (cons (evaltree-recursive one) (cons two '())))

     ;;If just two is a list , solve one and apply the expression to the result of two and the value of one, before creating a new list.
     (and (number? one) (and (list? two) (not= two (evaltree-recursive two))))
          (cons
            (apply (resolve(symbol exp)) (cons one (cons (first (evaltree-recursive two)) '())))
            (cons one (cons (evaltree-recursive two) '())))

     ;;else just return the list with the expression.
     :else
          (cons exp (cons one (cons two '())))
     )
    )
  )

;;A slightly better solution using nth instead of first and rest and using conj instead of multiple cons's.
;;Also solves list pairs where one cannot be solved.
(defn evaltree-recursive-v2
  ;;If we get a valid list, make a recursive call with the parts of the list as the arguments.
  ([list]
   (if (= 3 (count list))
     (evaltree-recursive-v2 (nth list 0) (nth list 1) (nth list 2))
     list
     ))

  ;;If we've got in an (exponent argument1 argument2) list
  ([exp one two]
   (cond
     ;;If both inputs are numbers, then apply the expression to one and two, and create a new list with the result replacing the expression.
     (and (number? one) (number? two))
      (cons
        (apply (resolve(symbol exp)) (conj '() two one))
        (conj '() two one))

     ;;If the inputs are lists, then apply the expression to the solutions of both lists, and create a new list with the result replacing the expression.
     (and (list? one) (list? two))
       (cond
         ;;If both are solvable
         (and (not= one (evaltree-recursive-v2 one)) (not= two (evaltree-recursive-v2 two)))
           (cons
             (apply (resolve(symbol exp)) (conj '() (first (evaltree-recursive-v2 two)) (first (evaltree-recursive-v2 one))))
             (conj '() (evaltree-recursive-v2 two) (evaltree-recursive-v2 one)))

         ;;If one is solvable
         (not= one (evaltree-recursive-v2 one))
          (conj '() two (evaltree-recursive-v2 one) exp)

         ;;If two is solvable
         (not= two (evaltree-recursive-v2 two))
          (conj '() (evaltree-recursive-v2 two) one exp)

         :else
          (conj '() two one exp)
         )

     ;;If just one is a list , solve one and apply the expression to the result of one and the value of two, before creating a new list.
     (and (number? two) (and (list? one) (not= one (evaltree-recursive-v2 one))))
      (cons
        (apply (resolve(symbol exp)) (conj '() two (first (evaltree-recursive-v2 one))))
        (conj '() two (evaltree-recursive-v2 one)))

     ;;If just two is a list , solve one and apply the expression to the result of two and the value of one, before creating a new list.
     (and (number? one) (and (list? two) (not= two (evaltree-recursive-v2 two))))
      (cons
        (apply (resolve(symbol exp)) (conj '() (first (evaltree-recursive-v2 two)) one))
        (conj '() (evaltree-recursive-v2 two) one))

     ;;else just return the list with the expression.
     :else
      (conj '() two one exp)
     )))


;;Issue is that second bracket doesn't get solved on last one. It probably could be solved.
 (defn evaltree-recursive-test []
   (assertequals '(6 3 2) (evaltree-recursive '(* 3 2)))
   (assertequals '(3 (3 1 2) (1 -5 6)) (evaltree-recursive '(* (+ 1 2) (+ -5 6))))
   (assertequals '(2 2 (1 -5 6)) (evaltree-recursive '(* 2 (+ -5 6))))
   (assertequals '(2 (1 -5 6) 2) (evaltree-recursive '(* (+ -5 6) 2)))
   (assertequals '(* () 2) (evaltree-recursive '(* () 2)))
   (assertequals '(* 2) (evaltree-recursive '(* 2)))
   (assertequals '(-52 (26 5 (21 3 7)) (-2 6 8)) (evaltree-recursive '(* (+ 5 (* 3 7)) (- 6 8))))
   (assertequals '(* (+ 5 ()) (- 6 8)) (evaltree-recursive '(* (+ 5 ()) (- 6 8)))))

(defn evaltree-recursive-v2-test []
  (assertequals '(6 3 2) (evaltree-recursive-v2 '(* 3 2)))
  (assertequals '(3 (3 1 2) (1 -5 6)) (evaltree-recursive-v2 '(* (+ 1 2) (+ -5 6))))
  (assertequals '(2 2 (1 -5 6)) (evaltree-recursive-v2 '(* 2 (+ -5 6))))
  (assertequals '(2 (1 -5 6) 2) (evaltree-recursive-v2 '(* (+ -5 6) 2)))
  (assertequals '(* () 2) (evaltree-recursive-v2 '(* () 2)))
  (assertequals '(* 2) (evaltree-recursive-v2 '(* 2)))
  (assertequals '(-52 (26 5 (21 3 7)) (-2 6 8)) (evaltree-recursive-v2 '(* (+ 5 (* 3 7)) (- 6 8))))
  (assertequals '(* (+ 5 ()) (-2 6 8)) (evaltree-recursive-v2 '(* (+ 5 ()) (- 6 8)))))


