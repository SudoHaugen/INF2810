; Oblig 1a inf2810


; skrevet av Marius Haugen 


; 1a

(* (+ 4 2) 5)

;Utskrift fra kjøring: 30
; Først vil subuttrykket bli evaluert (+ 4 2). Dermed vil vi anvende prosedyren (*) med argumentene 6 og 5. 

; 1b

(* (+ 4 2) (5))

;;Programmet vil ikke kjøre
; Det er fordi vi har et subuttrykk (5) som ikke er en prosedyre som kan anvendes til en verdi 

; 1c

(* (4 + 2) 5)

; Programmet kjører ikke
; Grunnen til at programmet ikke kjører er fordi vi har (4 + 2) som ikke er en gyldig prosedyre. I scheme evaluerer vi uttrykk
; fra venstre til høyre hvor den første verdien til venstre er prosedyren og resten er argumenter 

; 1d

(define bar (/ 42 2))
 bar

; Utskrift fra kjøring: 21
; Dette er en gyldig prosedyre ettersom define er en special form. Det vil si at den har andre regler som gjelder i sitt tilfelle
; I dette tilfellet assosierer vi bar med verdien av uttrykket (/ 42 2)

; 1e

(- bar 11)

; Utskrift fra kjøring: 10
; Nå som bar har blitt assosiert med en verdi kan vi bruke den som hvilken som helst annen numerisk verdi. I dette tilfellet
; har vi (- 21 11) 


; 1f

(/ (* bar 3 4 1) bar)

; Utskrift fra kjøring 12
; bar kan bli brukt på samme måte som andre numeriske verdier som en del av uttrykk. Verdien bar vil alltid bli evaluert til 21
; derfor kan vi bruke den som et del av et subbutrykk (/ (* 21 3 4 1) 21)


;----------------------------------------------------


; if, or, and og cond er eksempler på kondisjonale uttrykk som i Scheme er det vi kaller for en special form. Med andre ord
; har den egne evalueringsregler utenfor det som er standaren. Hvis vi har uttrykket (<p1> <e1>) vil <p1> bli evaluert til true / false først
; Hvis <p1> er false vil <e1> bli evaluert osv. 



; Oppgave 2a

(or (= 1 2)
     "piff!"
     "paff!"
     (zero? (1 - 1)))

; predikatet vil evaluerer alt som ikke er eksplisitt false til true. Derfor er outputet "piff!"


(and (= 1 2)
      "piff!"
      "paff!"
      (zero? (1 - 1)))

; and-predikatet vil evaluerer til false her fordi det første argumentet er false. and-predikatet evaluerer alt fra venstre til høyre
; og returnere den siste verdien hvis alle argumentene evaluere til true. Derimot er er det første argumentet her false og vil derfor
; returnere false. 

(if (positive? 42)
     "poff!"
    (i-am-undefined))

; If-predikatet evaluerer det første argumentet og sjekker om du får en true verdi. Her er det uttrykket som evalueres "poff!" fordi
; argumentet evaluerte til true. Hvis den evaluerte til false ville den ha gjort det samme med det andre
; paramateret som er (i-am-undefined). 


;Oppgave 2b

;Kjøring med if


(define (sign n)
  (if (< n 0)
     -1
  (if (= n 0)
     0
  1)))

(sign 1)
(sign 0)
(sign -1)


;Kjøring med cond


(define (sign n)
  (cond ((= n 0) 0)
       ((< n 0) -1)
       (else 1)))

(sign 1)
(sign 0)
(sign -1)

;Oppgave 2C


; Jeg brukte denne koden først for å eksperimentere med hvordan or statements fungerer

; or test
;  (define sign
;   (lambda (n)
;     (or (if (negative? n) -1
;         (if (positive? n) 1
;         (if (zero? n) 0))))))



; alt. 1

(define sign
  (lambda (n)
    (or (and (negative? n)
             -1)
        (and (positive? n)
              1)
        0)))

;alt. 2

(define (sign n)
  (or (and (< n 0)
           -1)
      (and (> n 0)
           1)
      0))

(sign 1)
(sign 0)
(sign -1)


;----------------------------


; Oppgave 3

; a)

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

; b)

(define plus
  (lambda (n x)
    (if (zero? x)
        n
        (plus (add1 n) (sub1 x)))))

(plus 5 4)



; c)



; I oppgave b skrev jeg en rekursiv prosedyre som ga opphav til en iterativ prosess ettersom den holder styr
; på verdiene til x og y gjennom hele prosessen
; f.eks: (plus 10 4)
; (10 + 1) + (4 - 1) = 11 + 3
; (11 + 1) + (3 - 1) = 12 + 2
; (12 + 1) + (2 - 1) = 13 + 1
; (13 + 1) + (1 - 1) = 14 + 0
; Dermed returnerer vi x 
; Det som kjenntegner en iterativ prosess er at den holder styr på faste variabler



(define plus
  (lambda (x y)
    (if (not (eq? y 0))
        (add1 (plus (sub1 y) x))
        x)))


; Denne prosessen gir opphav til en reklursiv prosess ettersom når vi kjører den vil vi få flere og flere operasjoner ventende
; i minnet. De ventende kallene vil ikke bli kjørt før vi har nådd basistilfellet
; feks (plus 10 4)
; 1 + ((4 - 1) + 10)
; 1 + 1 + ((4 - 1 - 1) 10)
; 1 + 1 + 1 + (4 - 1 - 1- 1) 10)
; 1 + 1 + 1 + 1 ((4 - 1 - 1 - 1 - 1) 10)
; Når vi evaluerer blir det 4 + (0 + 10) = 14
; Med denne voksende strukturen av prosedyrekall i minnet kan vi gjenkjenne dette som en rekursiv prosess 



; d)

; Vi kan forenkle uttrykket i power-iter fordi argumentene b og n allerede er definert i det leksikalse skopet til
; power-close-to. Power-iter har derfor direkte tilgang til disse argumentene som vil si at vi ikke trenger å sende dem
; på nytt som argumenter. Da blir den nye signaturen (power-iter e). Det fungerer også i dette tilfellet fordi argumentene
; b og n er konstante gjennom hele prosessen og trenger ikke å defineres på nytt.

(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (+ 1 e))))
  (power-iter 1))

(power-close-to 2 8)


; e)

; Helpeprosedyren fib-iter kan ikke forenkles i denne sammenhengen ettersom den tar inn argumenteter som alltid vil
; forandre verdi gjennom hver iterasjon. Hvis vi prøver å erstatte count med n vil det resultere i en evig loop når
; n er mer enn 0. Dette er igjen fordi n er en konstant verdi og vi trenger en teller som kan komme til 0 for å avslutte
;  if-testen vår. Derfor kan ikke hjelpeprosedyren forenkles. 


(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

