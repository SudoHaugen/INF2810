(load "oblig2btester.scm")

;; 1a

(define count 42)

(define make-counter
  (lambda ()
    (let ((count 0))
      (lambda ()
        (set! count (+ count 1))
        count))))

(define c (make-counter))

;; 1b - se tegning

;; 2a
(define (make-stack stack)

  (define (push! . items)
    (cond ((not (null? items))
        (let ((new (cons (car items) '())))
          (set-cdr! new stack)
          (set! stack new))
        (push! (cdr items)))))


  (define (pop!)
    (cond ((not (null? stack))
      (set! stack (cdr stack)))))

  (define (controller . args)
    (cond ((eq? (car args) 'push!) (push! (cdr args)))
          ((eq? (car args) 'pop!) (pop!))
          ((eq? (car args) 'stack) stack)))

  controller)

;; 2b
(define (push! s . items)
  (s 'push! items))

(define (pop! s) (s 'pop!))
(define (stack s) (s 'stack))

;; Kall
;(define test (make-stack (list 2 1)))
;(print (test 'stack))
;(test 'push! 3 4 5 6)
;(print (test 'stack))
;(test 'pop!)
;(print (test 'stack))

;(push! test 1000)
;(print (stack test))
;(pop! test)
;(print (stack test))

;; 3a
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

;; --- FØR ---
;;  _________   _________   _________   _________   _________
;;  | a | --|-->| b | --|-->| c | --|-->| d | --|-->| e | / |
;;  ---------   ---------   ---------   ---------   ---------

;; --- ETTER ---
;;  _________   _________   _________   _________   _________
;;  | a | --|-->| b | --|-->| c | --|-->| d | | |   | e | / |
;;  ---------   ---------   ---------   ------|--   ---------
;;                \__________________________/

;; list-ref går gjennom listen linjært og teller opp mot en indeks.
;; Siden cdr av det fjerde cons-paret peker mot det andre cons-paret,
;; vil "b" kunne refereres til med indeksene 1, 4, 7, 10, 13, ...

;; 3b
(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
(set-car! (car bah) 42)

;; --- FØR ---
;;  _____________   _________   _____________
;;  | bring | --|-->| a | --|-->| towel | / |
;;  -------------   ---------   -------------

;; --- ETTER ---
;;  _________   _________   _____________
;;  | | | --|-->| 42 | --|-->| towel | / |
;;  --|------   ---------   -------------
;;    \__________/

;; Det første elementet ser ut som resten av listen, fordi den peker dit.
;; car av resten av listen har blitt endret til 42.

;; 3c
(define (cycle? items)
  (define seen '())
  (define (iter items)
    (cond (
        (not (member (car items) seen))
          ((set! seen (append (cons (car items) '()) seen))
          (cycle? (cdr items))))
        (else #t)))
  (iter items))

(cycle? bah)

