(load "prekode3a.scm")

(define (simple-mem proc2)
  (let ((table (make-table)))
    (lambda x
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (apply proc2 x)))
              (insert! x result table)
              result))))))

(define original-procs (make-table))

(define (mem state proc)   
  (cond
    ((equal? 'unmemoize state)
     (or (lookup proc original-procs)
         proc))
    
    ((equal? 'memoize state)
     (let ((before proc)
           (after (simple-mem proc)))
       (insert! after before original-procs)
       after))
    
    (else (display "error!"))))

(test-proc)