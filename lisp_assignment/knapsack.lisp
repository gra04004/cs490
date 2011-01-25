;;;
;;; An instance of the knapsack problem, where a knapsack of a given capacity 
;;; W is to be filled with a subset of a given set of items with different 
;;; weights and values.
;;;
;;; The optimal solution is the subset of highest total value, whose total
;;; weight does not exceed W.  This is an optimization problem that 
;;; necessitates some constraint satisfaction while searching the solution 
;;; space.
;;;
;;;
;;; Learning Lisp has been a fun experience. It was frustrating at first
;;; because I never found clear answers to the questions I had. I had to
;;; infer the answer from example code. However, once I internalized the
;;; structure of the language, things became much more intuitive. I still
;;; would like to see a reference that clearly delineated the different
;;; built-in functions. But having a consistent list structure in every
;;; aspect of the language becomes elegant after some exposure.
;;;

(setf *instance*
  (list 101 ; maximum weight (W)
    (cons 94 85) ; item 1 weight and value
    (cons  6 26) ; item 2 ditto
    (cons 16 48) ; item 3
    (cons 92 21) ; item 4
    (cons 49 22) ; item 5
    (cons 37 95) ; item 6
    (cons 57 43) ; item 7
    (cons 15 45) ; item 8
    (cons 46 55) ; item 9
    (cons 22 52) ; item 10
    (cons 91  9) ; item 11
    (cons 59  1) ; item 12
    (cons 67 22) ; item 13
    (cons 98 94) ; item 14
    (cons  7 38) ; item 15
    (cons 44 74) ; item 16
    (cons 34 15) ; item 17
    (cons 66 82) ; item 18
    (cons 84 67) ; item 19
    (cons 93 84) ; item 20
  )
)

(setf *best* (list)) ; define the global variable *best*

(defun main()
  (backtrack nil 0) ; compute the most valuable set
  (show-best) ; display the most valuable set
)

(defun update-best (weights)
  (when (< (value-of *best*) (value-of weights))
    (setq *best* weights) ; if *best* has a smaller value than weights,
  )                       ; it is no longer the best!
)

(defun show-best ()
  (format T "A value of: ~A for the items: ~A" (value-of *best*) *best*)
)

(defun backtrack(weights level-num)
  (when (<= (total-weight-of weights) (max-weight))
    ; don't continue if the weight is too great, 
    ; any subnode would also be too heavy
    (if (< level-num (num-items))
      (progn ; if the level is less than the number of items
        (backtrack weights (+ level-num 1)) ; recurse without adding the
                                            ; current item
        (backtrack (append weights (list    ; and recurse with adding the
          (nth level-num (list-of-weights)))) (+ level-num 1) ; current item
        )
      )
      (update-best weights) ; otherwise we are at a leaf node so see if the 
                            ; sum of the values is better than best

      ; need not worry about the case where the level-num is greater
      ; than the num-items because no recursion takes place when they are
      ; equal, so that case never happens
    ) 
  )
)

(defun total-weight-of (weights)
  (apply #'+ weights)) ; sum all the weights in weights

;;; Helper functions to be called as necessary/desirable.

(defun num-items ()
  (length (cdr *instance*)))

(defun max-weight ()
  (car *instance*))

(defun list-of-weights ()
  (mapcar #'car (cdr *instance*)))

(defun list-of-values ()
  (mapcar #'cdr (cdr *instance*)))
  
(defun value-of-weight (weight)
  (cdr (assoc weight (cdr *instance*))))

(defun value-of (weights)
  (apply #'+ (mapcar #'value-of-weight weights)))


(main)