(load "huffman.scm")

(define (encode bits tree)
  (define (encode-1 bits current-branch)
    (if (null? bits)
        '()
          (if (eq? (member? eq? (car bits) (symbol-leaf (symbols (left-branch current-branch)))) #t)
              (cons (weight-leaf current-branch)
                    (encode-1 (cdr bits) tree))
              (encode-1 (cdr bits) (right-branch current-branch))
              )))
  (encode-1 bits tree))


(define (member? proc foo values)
  (cond ((null? values) #f)
        ((proc foo (car values)) #t)
        (else (member? proc foo (cdr values)))))