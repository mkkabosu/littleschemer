(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define double
  (lambda (x)
    (* x 2))) 

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define nil #f)

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) nil)
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))
