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



