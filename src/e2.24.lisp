;Exercise 2.24: 
;
;  Suppose we evaluate the expression
;
;  (list 1 (list 2 (list 3 4))).
;
;  Give the result printed by the interpreter, the corresponding 
;  box-and-pointer structure, and the interpretation of this as a tree 
;  (as in Figure 2.6).

;; The first answer is easiest, we just feed the expression to the interpreter.
(equal '(1 (2 (3 4))) (list 1 (list 2 (list 3 4))))

;; In box and pointer notation, we have the following
;;  ___ ___      ___ ___
;; |   | --|--->|   | n |
;;  -|- ---      -|- ---
;;   1            |
;;               ___ ___      ___ ___
;;              |   | --|--->|   | n |
;;               -|- ---      -|- ---
;;                2            |
;;                            ___ ___      ___ ___
;;                           |   | --|--->|   | n |
;;                            -|- ---      -|- ---
;;                             3            4
;;
;; As a tree, we see the following
;; 
;;                   (1 (2 (3 4)))
;;                         /\
;;                        /  \
;;                       /    \
;;                      1     (2 (3 4))
;;                               /\
;;                              /  \
;;                             /    \
;;                            2    (3 4)
;;                                   /\
;;                                  /  \
;;                                 /    \
;;                                3      4

