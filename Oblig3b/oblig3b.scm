(load "evaluator.scm")


;;Starter evaluatoren og REPLET 
(set! the-global-environment (setup-environment))





;;Oppgave 1 a)


(mc-eval '(define (foo cond else)
            (cond ((= cond 2) 0)
                  (else (else cond)))) the-global-environment)

(mc-eval '(define cond 3) the-global-environment)

(mc-eval '(define (else x) (/ x 2)) the-global-environment)

(mc-eval '(define (square x) (* x x)) the-global-environment)

(mc-eval '(foo 2 square) the-global-environment)

(mc-eval '(foo 4 square) the-global-environment)

(mc-eval '(cond ((= cond 2) 0)
                (else (else 4))) the-global-environment)



;; Det er mulig å definere variabler slik som cond og else ettersom at definisjoner lagres som quotede uttrykk i evaluatoren vår (tagged-list).
;; F.eks (define cond) blir det samme som 'cond i evaluatoren.

;; Det er også viktig å skille mellom hvilke omstendighet den er validert i. Slik cond er definert som en global variabel 3,
;; men den er også en lokal variabel i foo hvor den tilsvarer et argument. Det samme gjelder else. Special-formen til cond har også egne
;; syntaktiske krav som må oppfylles


;; cond er en global verdi 3, men cond kan er også et argument i foo, som gjør at verdien er lokal i foo. Det samme gjelder da else 


;; (foo 2 square):

;; Blir evaluert til 0 fordi cond argumentet i foo er 2, og ((= cond 2 ) 0)

;; (foo 4 square):

;; Programmet vil gjøre det samme som ovenfor, men denne gangen vil den gå inn i else og kalle prosedyreargumentet på cond-argumentet
;; Derfor returnerer den 16, (* 4 4)


;; (cond ((= cond 2) 0) (else (else 4))):

;; Her oppfyller cond det syntaktiske kravet til et kondisjonalt uttrykk, men argumentet cond henter den fra den globale omgivelsen ettersom
;; det er der den ble kalt. Else har også blitt kalt fra den globale-omgivelsen som vil da tilsvare define uttrykket for else
;; Den vil da evalueres til 2 (/ 4 2). 





;;Oppgave 2


;; a)


;; Legger til 1 på argumentet 

(define (1+ x)
  (+ x 1))


;; Trekker fra 1 på argumentet

(define (1- x)
  (- x 1))



;; Her er koden som vi implementerte i kildekoden til evaluatoren 


;; b)

(define (install-primitive! proc body)
  (set-car! (car the-global-environment)
            (append (caar the-global-environment) (list proc)))
  (set-cdr! (car the-global-environment)
            (append (cdar the-global-environment) (list (list 'primitive body)))))


(install-primitive! 'square (lambda (x) (* x x)))



;; Oppgave 3


;; a)


;; Dette ble satt inn i evaluatorens kildekode

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((and? exp) (eval-and exp env)) ;;oppg 3a
        ((or? exp) (eval-or exp env)) ;;oppg 3a
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ))

(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; oppg 3a
        ((or? exp) #t) ;; oppg 3a
        (else #f)))

(define (and? exp) (tagged-list? exp 'and))

(define (or? exp) (tagged-list? exp 'or))



(define (eval-and exp env)
  (if (null? (cdr exp)) #t
      (iter-and exp env)))

(define (iter-and exp env)   
  (if (and (null? (cdr exp))
           (true? (mc-eval (car exp) env)))
      (car exp)
      (cond ((equal? 'and (car exp))
             (iter-and (cdr exp) env))
            ((true? (mc-eval (car exp) env))
             (iter (cdr exp) env))
            (else #f))))



(define (eval-or exp env)
  (if (null? exp)
       #f
       (cond ((equal? 'or (car exp))
              (eval-or (cdr exp) env))
             ((true? (mc-eval (car exp) env))
              (car exp))
              (else (eval-or (cdr exp) env)))))




;; Oppgave 3b

(set! if-consequent
      (lambda (exp)
        (if (tagged-list? (cddr exp) 'then) (cadddr exp))))

(set! if-alternative
      (lambda (exp)
        (if (not (null? (cddddr exp)))
            (if (tagged-list? (cdr (cdddr exp)) 'elseif)
                (cons 'if (cdr (cddddr exp)))
                (car (cdr (cddddr exp))))
            'false)))

(mc-eval '(if #t
              then 12
              elseif #f
              then 54
              elseif #f
              then 19
              else 28) the-global-environment)
(mc-eval '(if #t
              then (= 1 1)
              elseif #f
              then #f
              else 22) the-global-environment)


;; Oppgave 3c


;; Legger til let som tilfelle i eval-special-form
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and exp env)) 
        ((or? exp) (eval-or exp env))
        ((let? exp) (mc-eval (let->lambda exp) env)))) ;; Oppg 3 c


;; Legger til let som tilfelle i special-form?
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t)
        ((or? exp) #t)
        ((let? exp) #t) ;; Oppg 3c
        (else #f)))


;; Legger til predikatet let?
(define (let? exp) (tagged-list? exp 'let))


;; Lager prosedyrer for å evaluere let-uttrykk:

(define (let-clauses exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let->lambda exp)
  (make-lambda-exp (let-clauses exp) (let-body exp)))


;; Lager en liste med alle variablene
(define (make-var-list clauses)
  (if (null? clauses)
      '()
      (let ((var (caar clauses))
            (rest (cdr clauses)))
        (cons var (make-var-list rest)))))

;; Lager en liste med alle uttrykkene
(define (make-exp-list clauses)
  (if (null? clauses)
      '()
      (let ((exp (cadar clauses))
            (rest (cdr clauses)))
        (cons exp (make-exp-list rest)))))


;; Lager og returnerer et lambda-uttrykk
(define (make-lambda-exp clauses body)
  (if (null? clauses)
      'false
      (cons (make-lambda (make-var-list clauses) body)
            (make-exp-list clauses))))



; Oppgave 3d


(define (let-clauses exp)
  (if (list? (cadr exp))
      (cadr exp)
      (alt-let-clauses exp)))

(define (alt-let-clauses exp)
  (if (equal? 'in (car exp))
      '()
      (cons (car exp) (alt-let-clauses (cdr exp)))))

(define (let-body exp)
  (if (list? (cadr exp))
      (cddr exp)
      (alt-let-body exp)))

(define (alt-let-body exp)
  (if (equal? 'in (car exp))
      (cdr exp)
      (alt-let-body (cdr exp))))



(define (make-var-list clauses)
  (if (null? clauses)
      '()
      (if (list? (car clauses))
          (let ((var (caar clauses))
                (rest (cdr clauses)))
            (cons var (make-var-list rest)))
          (let ((var (cadr clauses))
                (rest (cddddr clauses)))
            (cons var (make-var-list rest))))))

(define (make-exp-list clauses)
  (if (null? clauses)
      '()
      (if (list? (car clauses))
          (let ((exp (cadar clauses))
                (rest (cdr clauses)))
            (cons exp (make-exp-list rest)))
          (let ((exp (cadddr clauses))
                (rest (cddddr clauses)))
            (cons exp (make-exp-list rest))))))


; Oppgave 3e


;; Legger til let som tilfelle i eval-special-form
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and exp env)) 
        ((or? exp) (eval-or exp env))
        ((let? exp) (mc-eval (let->lambda exp) env))
        ((while? exp) (eval-while exp env)))) ;;oppg 3e


;; Legger til let som tilfelle i special-form?
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t)
        ((or? exp) #t)
        ((let? exp) #t)
        ((while? exp) #t) ;;oppg 3e
        (else #f)))


;;legger til while predikatet 
(define (while? exp) (tagged-list? exp 'while))


;; Evaluator for while 

(define (eval-while exp env)
  (define (iter condition exps)
    (if (and (not (null? exps)) condition)
        (begin (car exps)
               (iter condition (cdr exps)))))
  (if (null? exp)
      #f
      (let ((condition (cadr exp))
            (rest (caddr exp)))
        (iter condition rest))))


(define (while-pred exp)
  (cadr exp))

(define (while-body exp)
  (caddr exp))

(define (while->sequence exp)
  (if (null? exp)
      '()
      (if (while-pred exp)
          (cons (while-body exp)
                (while->sequence exp))
          #f)))

(define (while predicate body)
  (let ((pred predicate))
    (if predicate
        (begin
          body
          (while pred body))
        #f)))