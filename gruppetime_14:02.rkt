(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x 6))
(define b (cons 7 8))
(define w (cons x y z))

(define w (cons (list x y) (list z b)))


(define (make-replacer x y)
  (lambda (z)
    (if (= z x)
        y
        z)))


(define 1s-to-2s
  (make-replacer 1 2))

(1s-to-2s 5)

(define (compose proc1 proc2)
  (lambda (x)
    (proc1 (proc2 x))))


((compose zero? string->number) "0")

(define not zero?
  (compose not zero?))

(define (negate pred)
  (compose not pred))

(let ((variabel verdi)