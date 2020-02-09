(load "huffman.scm")

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


; Oppgave 2

; a)

(define (member? proc foo values)
  (cond ((null? values) #f)
        ((proc foo (car values)) #t)
        (else (member? proc foo (cdr values)))))

(define (member? proc foo values)
  (if (null? values)
      #f
      (if (proc foo (car values))
             #t
             (member? proc foo (cdr values)))))


(define (member? pred elem items)
  (cond
    ((null? items) #f)
    ((pred elem (car items)) #t)
    (else (member? pred elem (cdr items)))))

; b)


(define (decode bits tree)
  (define (decode-1 bits current-branch accum)
    (if (null? bits)
        accum
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cdr bits) tree (append accum (list (symbol-leaf next-branch))))
              (decode-1 (cdr bits) next-branch accum)
              ))))
  (decode-1 bits tree '()))


; c)

; (ninjas fight ninjas by night)


; d)

(define (encode bits tree)
  (define (encode-1 bits current-branch accum)
    (if (null? bits)
        accum
        (let ((next-branch (car (choose-branch2 (car bits) current-branch))))
          (if (leaf? next-branch)
              (encode-1 (cdr bits) tree (append accum (cdr (choose-branch2 (car bits) current-branch))))
              (encode-1 bits next-branch (append accum (cdr (choose-branch2 (car bits) current-branch))))))))
  (encode-1 bits tree '()))


(define (choose-branch2 symbol branch)
  (if (member? equal? symbol (symbols (left-branch branch)))
      (list (left-branch branch) 0)
      (list (right-branch branch) 1)))


; e)


               
(define (grow-huffman-tree input)
  (define (grow-huffman-tree-1 reverse-input)
    (if (null? (cddr reverse-input))
        (make-code-tree (car reverse-input) (cadr reverse-input))
        (make-code-tree (car reverse-input)
                        (grow-huffman-tree-1 (cdr reverse-input)))))
  (grow-huffman-tree-1 (reverse (make-leaf-set input))))


(define freqs '((a 2)
                (b 5)
                (c 1)
                (d 3)
                (e 1)
                (f 3)))

(define codebook (grow-huffman-tree freqs))

(decode (encode freqs codebook) codebook)


; f)


(define freqs-2 '((samurais 57)
                  (ninjas 20)
                  (fight 45)
                  (night 12)
                  (hide 3)
                  (in 2)
                  (ambush 2)
                  (defeat 1)
                  (the 5)
                  (sword 4)
                  (by 12)
                  (assassin 1)
                  (river 2)
                  (forest 1)
                  (wait 1)
                  (poison 1)))

(define temp-list '(ninjas fight ninjas fight ninjas ninjas fight samurais samurais fight samurais fight ninjas ninjas fight by night))


(define codebook2 (grow-huffman-tree freqs-2))

; 1.

; Laget en metode for å hjelpe oss å telle antall bits

(define (list-counter x)
  (if (null? x)
      0
      (+ 1 (list-counter (cdr x)))))

      
(decode (encode freqs-2 codebook2) codebook2)


;(list-counter (encode temp-list codebook2)) -> 42.
; Det er altså 42 bits som brukes for å kode meldingen




; 2.

(/ (list-counter (encode temp-list codebook2)) 17)

; 42 / 17 = 2,47
; Den gjennomsnittelige lengden er på 2,47 (ettersom det er 17 elementer i listen)


; 3.

; 4*17 = 68 bits


; g)


(define (huffman-leaves branch)
      (if (leaf? branch)
          (list (list (symbol-leaf branch) (weight-leaf branch)))
          (append '()
            (append (huffman-leaves (left-branch branch))
                    (huffman-leaves (right-branch branch))))))


; h)
      

; f)

(define (reduce proc init items)
    (if (null? items)
        init
        (reduce
          proc
          (proc (car items) init)
          (cdr items))))

(define (sum items) (reduce + 0 items))


(define (expected-code-length tree)
  (define tree-weight (weight tree))
  (define (prob-list branch count)
      (if (leaf? branch)
        (list (* (/ (weight-leaf branch) tree-weight) count))
        (append '()
          (append (prob-list (left-branch branch) (+ 1 count))
                  (prob-list (right-branch branch) (+ 1 count))))))
  (sum (prob-list tree 0)))
  
  
    
    












