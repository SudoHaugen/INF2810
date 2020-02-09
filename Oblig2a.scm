; Oppgave 1

; a)

(define (p-cons x y)
  (lambda (proc)
    (proc x y)))

(define (p-car p)
  (p (lambda (x y)
       x)))

(define (p-cdr p)
  (p (lambda (x y)
       y)))

; b)

(define foo 42)

(let ((foo 5)
      (x foo))
  (if (= x foo)
      'same
      'different))

((lambda (foo x)
 (if (= x foo)
     'same
     'different))
 5 foo)



(let ((bar foo)
      (baz 'towel))
  (let ((bar (list bar baz))
        (foo baz))
    (list foo bar)))

((lambda (bar baz)
   ((lambda (bar foo)
     (list foo bar))
    (list bar baz) baz))
   foo 'towel)



      
; c)

(define (infix-eval exp)
  ((car (cdr exp)) (car exp) (car (cdr (cdr exp)))))

(define bah '(84 / 2))



; d)


; Programmet kjører ikke. Dette blir åpenbart når vi kjører kommandoen (car (cdr bah)). Her får vi en tilbakemelding som
; sier at / ikke er en prosedyre, men et symbol. Vi kan ikke anvende et symbol på argumentene. 













 
                 