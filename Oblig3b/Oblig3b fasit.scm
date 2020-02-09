;; idasmot, tajaz, tonjro
;; Oblig 3b

(load "evaluator-fasit.scm")

(set! the-global-environment (setup-environment))
(define glob the-global-environment)
(define repl read-eval-print-loop)

;;=================== Oppgave 1 a) ===================;;

(mc-eval '(define (foo cond else)
            (cond ((= cond 2) 0)
                  (else (else cond))))
         glob)


(mc-eval '(define cond 3)
         glob)


(mc-eval '(define (else x) (/ x 2))
         glob)


(mc-eval '(define (square x) (* x x))
         glob)


(mc-eval '(foo 2 square) glob)
;; Evalueres til 0.
;; Tallet 2 blir sendt som argumentet 'cond.
;; Og siden (= cond 2) evalueres til 0 er det dette som blir vår returverdi.


(mc-eval '(foo 4 square) glob)
;; Evalueres til 16.
;; Siden tallet 4 blir sendt til 'cond og feiler,
;; er det else i (foo cond else) som gjelder. I dette tilfellet
;; er else square, som gir oss (* 4 4), som er 16. 


(mc-eval '(cond ((= cond 2) 0) 
                (else (else 4))) 
         glob)
;; Evalueres til 2.
;; Siden cond er definert til å være 3, vil cond i cond-uttrykket
;; feile, og else vil gjelde. Else er allerede definert som
;; (/ x 2) som i vårt tilfelle gir (/ 4 2), som er 2. 




;;=================== Oppgave 2 a) ===================;;

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
        ;;      her kan vi legge til flere primitiver.
        
        ;;      Endringer:
        (list '1+
              (lambda (x) (+ x 1)))
        (list '1-
              (lambda (x) (- x 1)))
        ))

;;; MC-Eval input: (1+ 2)
;;; MC-Eval value: 3

;;; MC-Eval input: (1- 2)
;;; MC-Eval value: 1



;;=================== Oppgave 2 b) ===================;;

(define (install-primitive! symbol val)
  (define-variable! symbol (list 'primitive val)
    the-global-environment))



;;=================== Oppgave 3 a) ===================;;

;; Legger til and og or som tilfeller i eval-special-form
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
        ((and? exp) (eval-and exp env)) ;; <---------- her
        ((or? exp) (eval-or exp env)))) ;; <---------- og her


;; Legger til and og or som tilfeller i special-form?
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; her
        ((or? exp) #t)  ;; og her
        (else #f)))


;; Legger til predikatene and? og or?

(define (and? exp) (tagged-list? exp 'and))

(define (or? exp) (tagged-list? exp 'or))


;; Legger til prosedyrer for å evaluere utrykkene

;; and:
(define (eval-and exp env)
  (if (and (null? (cdr exp))
           (true? (mc-eval (first-exp exp) env)))
      (first-exp exp)
      (cond ((equal? 'and (first-exp exp))
             (eval-and (next-exp exp) env))
            ((true? (mc-eval (first-exp exp) env))
             (eval-and (next-exp exp) env))
            (else #f))))


;; or:
(define (eval-or exp env)
  (if (null? exp)
      #f
      (cond ((equal? 'or (first-exp exp))
             (eval-or (next-exp exp) env))
            ((true? (mc-eval (first-exp exp) env))
             (first-exp exp))
            (else (eval-or (next-exp exp) env)))))


;; hjelpeprosedyrer:
(define (first-exp exp) (car exp))

(define (next-exp exp) (cdr exp))


;;=================== Oppgave 3 b) ===================;;

;; Endrer eval-if til å ta hensyn til at (if-alternative exp)
;; kan være elsif:

(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (if (elsif? exp)
          (eval-if (next-if-exp exp) env)
          (mc-eval (if-alternative exp) env))))


;; Lager en prosedyre som plukker ut elsif uttrykket
;; og endrer if-predicate, if-consequent og
;; if-alternative til å ta hensyn til den nye syntaksen:

(define (next-if-exp exp) (cddddr exp))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (cadddr exp))

(define (if-alternative exp)
  (if (not (null? (cddddr exp)))
      (cadr (cddddr exp))
      'false))


;; Definerer et elsif? predikat:
(define (elsif? exp)
  (if (tagged-list? (next-if-exp exp) 'elsif)
      #t
      #f))


;; Kjøreeksempler:

;;; MC-Eval input:
;; (if (and 1 #f)
;;      then 1
;;      elsif (and 1 2)
;;      then 2
;;      else 3)
;;
;;; MC-Eval value:
;; 2


;;; MC-Eval input:
;; (if (and 1 #f)
;;      then 1
;;      elsif (and #f 2)
;;      then 2
;;      else 3)
;;
;;; MC-Eval value:
;; 3

;;=================== Oppgave 3 c) ===================;;

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
        ((let? exp) (mc-eval (let->lambda exp) env)))) ;; her


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
        ((let? exp) #t) ;; her
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


;; Kjøreeksempler:

;;; MC-Eval input:
;; (let ((x 2) (y 3)) (* x y))
;;
;;; MC-Eval value:
;; 6


;;=================== Oppgave 3 d) ===================;;

;; Endrer let-clauses og let-body til å ta hensyn
;; til den nye syntaksen, med hjelpemetoder for å behandle
;; den alternative syntaksen:

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


;; Endrer make-var-list og make-exp-list slik at
;; de også tar hensyn til alternativ syntaks:

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


;;; MC-Eval input:
;; (let x = 2 and y = 3 in (display (cons x y)) (+ x y))
;;
;; (2 . 3)
;;
;;; MC-Eval value:
;; 5


;;=================== Oppgave 3 e) ===================;;

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
        ((while? exp) (eval-while exp env))))        ;;her


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
        ((while? exp) #t)     ;;her
        (else #f)))


;; Legger til predikatet let?
(define (while? exp) (tagged-list? exp 'while))

#|
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
|#

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


;;=================== Oppgave 4 :)  ===================;;




;;=====================================================;;

(set! the-global-environment (setup-environment))
(define glob the-global-environment)
(define repl read-eval-print-loop)