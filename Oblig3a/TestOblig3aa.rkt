(load "prekode3a.scm")

(define (mem proc)
  (let ((mem-table (make-table)))
    (lambda k
      (let ((prev-result (lookup k mem-table)))
        (or prev-result
            (let ((result (apply proc k)))
              (insert! k result mem-table)
              result))))))

(define proc-records (make-table))

(define (mem2 message proc)
  (cond
    ((equal? 'unmemoize message)
     (display "Unmemoziing...") (newline)
     (or (lookup proc proc-records)
         proc))
    
    ((equal? 'memoize message)
     (let ((old proc)
           (new (mem proc)))
       (display "Memoziing...") (newline)
       (insert! new old proc-records)
       new))
    (else (display "Error"))))


(set! fib (mem2 'memoize fib))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)

(set! fib (mem2 'unmemoize fib))

(fib 2)

(test-proc)

(test-proc 1 2 3 4)