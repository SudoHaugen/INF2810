; Oppgave 1


; f)

(define bar "Hello") ;Måtte gi bar en definisjon for at den skulle kjøre i replet

(car (cdr (list 0 42 #t bar)))


; g)  

(car (cdr (car '((0 42) (#t bar)))))


; h)

(car (car (cdr '((0) (42 #t) (bar)))))

; i)

; Ved bruk av bare cons:

(cons '(0)
      (cons '(42 #t)
            (cons '(bar)
                  '())))
                  

; ved bare bruk av list


(list '(0 42) '(#t bar))


; Oppgave 2


; a)


(define (length2 items)
  (define (length_count items y)
    (if (null? items)
        y
        (length_count (cdr items)
                      (+ y 1))))
  (length_count items 0))

; b)


; Prosedyren gir opphav til en halerekursiv prosedyre som er definert i hjelpeprosedyren reverse som kaller på seg selv til slutt
; Dette ga opphav til en iterativ prosess ettersom hver gang vi lager et nytt kall på reverse vil den returnere verdien
; til den originale kalleren og er derfor heller ikke avhengig av stack minnet.

(define (reduce-reverse proc init items)
  (define (reverse input output)
    (if (null? input)
        output
        (reverse (cdr input)
                 (cons (car input) output))))
  (reverse items init))


; c)

; Vanlig prosedyre

(define (all? proc items)
  (cond ((null? items) #t)
        ((proc (car items))
         (all? proc (cdr items)))
        (else #f)))

; Anonym prosedyre

(all? (lambda (x)
   (<= x 10))
 '(1 2 3 4 50))


(all?
 (lambda (items)
   (cond ((null? items) #t)
         ((> items 10) #f)))
 '(1 2 3 4 5))



; d)

(define (nth index items)
  (define (counter x items)
    (if (eq? index x)
        (car items)
        (counter (+ 1 x) (cdr items))))
  (counter 0 items))

; e)

(define (where index items)
  (define (counter x items)
    (cond ((null? items) #f)
          ((eq? index (car items)) x)
          (else (counter (+ 1 x) (cdr items)))))
  (counter 0 items))


; f)

(define (map2 proc items items2)
  (define (make-list items items2 output)
    (cond ((null? items) output)
          ((null? items2) output)
          (else (cons (proc (car items) (car items2))
                 (make-list (cdr items) (cdr items2) output)))))
  (make-list items items2 '()))


; g)

(map2
 (lambda (items items2)
   (cond ((null? items) '()) 
         ((null? items2) '())
         ((/ (+ items items2) 2))))
 '(1 2 3 4) '(3 4 5))



; h)

(define (both? proc)
  (lambda (x y)
    (and (proc x)
         (proc y))))


(map2 (both? even?) '(1 2 3) '(1 2 3))


; i)

(define (self proc)
  (lambda (x)
    (proc x x)))