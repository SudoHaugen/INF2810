;;Oppgave 1 Grunnleggende

;; Se vedlegg


;;Oppgave 2 Mysterium


(define (x y z)
  (or (null? z) (and (y (car z))
                     (x y (cdr z)))))

(x positive? '(1 2 3))

#|

Prosedyren tar inn et predikat og en liste med argumenter som den
rekursivt anvender på alle plasseringene i listen som er sendt med
som argument og returnerer #f dersomer et element i lista tester usant
for predikatet.

(Burde ha skrevet her at x er en høyere ordensprosedyre)


Eksempel på kall:

(x positive? '(1 2 3))

returnerer #t

|#



;; Oppgave 3 Rekursjon



(define (avg first . rest)
  (let ((arguments (cons first rest)))
    (define (iter args sum count)
      (if (null? args)
          (/ sum count)
          (iter (cdr args) (+ sum (car args)) (+ 1 count))))
    (iter arguments 0 0)))

(avg 1 2 3)


#|
Ifølge fasiten trenger jeg ikke å ha et let uttrykk for å lage en liste
av argumentene.

Følgende kall kunne ha blitt brukt istedenfor (uten å sende med arguments)

(iter first 1 rest)
|#


;; Oppgave 6 Evalueringsstrategier

#|

Kort fortalt er hovedforskjellen mellom applicative-order evaluation og lazy-evaluation er at i
applicative-order evaluation må alle argumenter evalueres før en prosedyre kan anvendes, mens i lazy
evaluation utsetter vi evalueringen til vi faktisk trenger verdiene deres.

Applicative-order evaluation er standardstrategien for evaluering i Scheme og er det vi har brukt
hovedsakelig i faget vårt.

Når det gjelder lazy-evaluation har vi særlig brukt dette i strømmer, memoisering og vår metasirkulære
evaluator.

- I den metasirkulære evaluatoren så vi på hvordan vi kunne bruke lazy evaluation som vår hovedstrategi

|#


;; Oppgave 7 Dataabstraksjoner og strømmer


;; Oppgave A)

;; Lager flat liste av alle nodeverdiene i et tre

(define (fringe tree)
  (cond ((null? tree) '())
        ((pair? (car tree))
         (append (fringe (car tree))
                 (fringe (cdr tree))))
        (else (cons (car tree)
                    (fringe (cdr tree))))))


;; Sjekker om alle nodeverdiene er like

(define (same-fringe? x y pred)
  (define (iter x1 y1)
    (cond ((and (null? x1) (null? y1)) #t) ;; Sjekker om begge listene er tomme #t
          ((or (null? x1) (null? y1)) #f) ;; Sjekker om en av listene er tomme #f
          ((not (pred (car x1) (car y1))) #f) ;; Sjekker om verdiene ikke er like
          (else (iter (cdr x1) (cdr y1))))) ;; Kaller på iter hvis verdiene er like 
  (iter (fringe x) (fringe y))) ;; Kaller på fringe for å få en flatet liste 


;; Oppgave B)

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-append
                                   (stream-cdr s1) s2)))))


(define (fringe-stream tree)
  (cond ((null? tree) the-empty-stream)
        ((pair? (car tree))
         (stream-append (fringe-stream (car tree))
                        (fringe-stream (cdr tree))))
        (else (cons-stream (car tree)
                           (fringe-stream (cdr tree))))))


(define (same-fringe-stream? x y pred)
  (define (iter x1 y1)
    (cond ((and (stream-null? x1) (stream-null? y1)) #t)
          ((or (stream-null? x1) (stream-null? y1)) #f)
          ((not (pred (stream-car x1) (stream-car y1))) #f)
          (else (iter (stream-cdr x1) (stream-cdr y1)))))
  (iter (fringe-stream x) (fringe-stream y)))



;; Oppgave C)


;; Hvor mange cons celler generes og hvorfor?

(fringe-stream '(1 ((2) 3 4)))

(fringe-stream '((1 2) 3 4))





;; Oppgave 8 Kallstatistikk



(define original fringe)

(define (fringe tree)
  (cond ((null? tree) '())
        ((pair? (car tree))
         (append (fringe (car tree))
                 (fringe (cdr tree))))
        (else (cons (car tree)
                    (fringe (cdr tree))))))



(define (monitor procedure)
  (let ((count 0))
    (lambda args
      (let ((message (and (not (null? args))
                              (car args))))
        (cond ((eq? message 'zero) (set! count 0))
              ((eq? message 'reset) procedure)
              ((eq? message 'count) count)
              (else (set! count (+ count 1))
                    (apply procedure args)))))))



(define original (fringe '((1 2) 3 4)))

(set! fringe (monitor fringe))


(fringe '((1 2) 3 4))



(define foo '(2, 3, 3, 4))
(define bar (cons (car foo) (cdr foo)))



(define (take n l)
  (define (iter count new-l)
    (cond ((null? new-l) new-l)
          ((= count 0) new-l)
          (else (iter (- count 1) (cdr new-l)))))
  (iter n l))


;; Fin måte å reversere en liste på rekursivt

(define (reverse liste)
  (if (null? liste)
      liste
      (append (reverse (cdr liste)) (list (car liste)))))



(define (cons x y)
  (lambda (message)
    (cond ((eq? message 'car) x)
          ((eq? message 'cdr) y)
          ((eq? message 'set-car!) (lambda (z) (set! x z)))
          ((eq? message 'set-cdr!) (lambda (v) (set! y v))))))

(define (set-car! liste new-value)
  ((liste 'set-car!) new-value))

(define (set-cdr! liste new-value)
  ((liste 'set-cdr!) new-value))
  
(define (car p)
  (p 'car))
(define (cdr p)
  (p 'cdr))


(define (list? items)
  (or (null? items)
      (and (pair? items)
           (list? (cdr items)))))


(define (deep-map proc nested)
  (cond ((null? nested) nested)
        ((not (list? nested) 
        ((list? (car nested) (deep-map proc (car nested))))
        (else (deep-map proc (cdr nested)))))))

(define (deep-map proc nested)
  (if (null? nested)
      nested
      (if (list? (car nested))
          (deep-map proc (car nested))
          (apply proc (car nested)
          (deep-map proc (cdr nested))))))


(define (deep-map proc nested)
  (cond ((null? nested) '())
        ((pair? nested)
         (cons (deep-map proc (car nested))
               (deep-map proc (cdr nested))))
        (else (proc nested))))
          


    
  
      














