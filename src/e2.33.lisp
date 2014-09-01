;Exercise 2.33: 
;
;  Fill in the missing expressions to complete the following definitions of 
;  some basic list-manipulation operations as accumulations:
;  
;  (define (map p sequence)
;    (accumulate (lambda (x y) <??>) nil sequence))
;  (define (append seq1 seq2)
;    (accumulate cons <??> <??>))
;  (define (length sequence)
;    (accumulate <??> 0 sequence))

;; Common Lisp doesn't have a direct equivalent to accumulate.  The loop and
;; iterate macros do perform accumulation via certain keywords, but their use
;; is both not in the spirit of this problem and beyond the scope her.  The
;; text provides an implementation of accumulate in section 2.2.3 that we can
;; use here
;;
(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence)
                (accumulate op initial (cdr sequence))))) 

;; Now that we have accumulate, we can begin our solution
;;
;; For map, we need an accumulation function that applies the passed
;; operation p to the initial value conses it with the remaining values 
(defun my-map (p sequence)
  (accumulate (lambda (x y) (cons (funcall p x) y)) nil sequence))

;; Test
(equal '(1 4 9) (my-map (lambda (x) (* x x)) (list 1 2 3)))
(equal '(4 16 36) (my-map (lambda (x) (* x x)) (list 2 4 6)))

;; For append, we need to place sequence 1 after sequence 2.  We can
;; accomplish this by consing seq2 with seq1
(defun my-append (seq1 seq2)
  (accumulate #'cons seq2 seq1))

;; Test
(equal '(1 2 3 4 5 6) (my-append (list 1 2 3) (list 4 5 6)))

;; For length, we add one to the initial value for each element in the list.
(defun my-length (sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(= 3 (my-length (list 1 2 3)))
(= 5 (my-length (list 1 2 3 4 5)))

