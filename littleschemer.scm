(define fact
  (lambda (x)
    (cond
     ((zero? x) 1)
     (else (* #?=x (fact (- x 1)))))))
