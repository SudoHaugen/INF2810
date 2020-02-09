; Gruppetime 7. Februar - INF2810

; Lister og listerekursjon
; Gitt en liste med naturlige tall, finn antallet partall i listen

(define (find-even items)
         (cond ((null? items) 0)
               ((even? (car items))
                (+ 1 (find-even (cdr items))))
               (else (find-even (cdr items)))))

;; Returnerer en ny liste med bare partall

(define (make-even items)
  (cond ((null? items) '())
        ((even? (car items) (make-even (cdr items))))
        (else (make-even (cdr items)))))

;Høyereordens-prosedyrer

(define add1 (lambda (x) (+ x 1)))
(define (increment proc x) (proc x))


(define words '("høyereordens" "prosedyrer" "i" "scheme" "er" "prosedyrer" "som"
  "tar" "andre" "prosedyrer" "som" "et" "argument" "eller" "returnerer"
"prosedyrer" "som" "returverdi"))

(define (is-word? word)
  (lambda (x) (equal? word x)))

(define (count-word word)
  (reduce + 0
          (map (lambda (x) 1)
(filter (is-word? word) words))))