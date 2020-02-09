(define (deep-map proc nested)
  (cond ((null? nested) '())
        ((pair? nested)
         (cons (deep-map proc (car nested))
               (deep-map proc (cdr nested))))
        (else (proc nested))))



(define (replace x y seq)
  (define (iter new-seq)
    (cond ((null? new-seq) '())
          ((not (equal? (car sq)))))))







(define (nested-match x tree)
  (cond ((null? tree) 0)
        ((pair? (car tree))
         (+ (nested-match x (car tree))
            (nested-match x (cdr tree))))
        ((eq? x (car tree))
         (+ 1 (nested-match x (cdr tree))))
        (else (nested-match x (cdr tree)))))



(let ((foo (list 1 2))
      (bar (* 2 2)))
  (cons bar foo))



(lambda (foo)
  (lambda (bar)
    (cons ((list 1 2) (* 2 2))))
  foo)


(define (compose p1 p2)
  (lambda (x)
    (p1 (p2 x))))


(define (add1 x)
  (+ x 1))

(define (add100 x)
  (+ x 100))


(define (repeat p n)
  (if (= n 1)
      p
      (compose p (repeat p (- n 1)))))


(define (eval-influx liste)
  ((cadr liste) (car liste) (caddr liste)))


(define exp1 (list 1 + 3))
(define exp2 (list 10 / 5))


(define (scale x seq)
  (if (null? seq)
      '()
      (* x (scale x (cdr seq)))))

(define (scale x seq)
  (define (iter in out)
    (if (null? in)
        (reverse out)
        (iter (cdr in)
              (cons (* (car in) x) out))))
  (iter seq '()))

