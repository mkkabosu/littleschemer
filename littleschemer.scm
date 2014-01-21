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

(define rember
  (lambda (a lat)
    (cond
     ((null? lat)(quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
		 (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l))
	    (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond 
	    ((eq? (car lat) old) 
	     (cons old
		   (cons new (cdr lat))))
	    (else (cons (car lat)
			(insertR new old
				 (cdr lat)))))))))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat)(quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons new
		   (cons old (cdr lat))))
	    (else (cons (car lat)
			(insertL new old 
				 (cdr lat)))))))))
(define subst
  (lambda (new old lat)
    (cond 
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons new (cdr lat)))
	    (else (cons (car lat)
			(subst new old
			       (cdr lat)))))))))
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((or (eq? (car lat) o1) (eq? (car lat) o2))
	    (cons new (cdr lat)))
	     (else (cons (car lat)
			(subst2 new o1 o2
				(cdr lat)))))))))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) a)
	(multirember a (cdr lat)))
       (else (cons (car lat)
		   (multirember a
				(cdr lat)))))))))
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
    (else
     (cond
      ((eq? (car lat) old)
       (cons (car lat)
	     (cons new
		   (multiinsertR new old
				 (cdr lat)))))
      (else (cons (car lat)
		  (multiinsertR new old
				(cdr lat)))))))))

(define multiinsertL
  (lambda (new old lat)
    (cond 
     ((null? lat) (quote ()))
    (else
     (cond
      ((eq? (car lat) old)
       (cons new
	     (cons old
		   (multiinsertL new old
				 (cdr lat)))))
      (else (cons (car lat)
		  (multiinsertL new old
				(cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons new
		   (multisubst new old 
			       (cdr lat))))
	   (else (cons (car lat)
		       (multisubst new old
				   (cdr lat)))))))))
(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup) (addtup (cdr tup)))))))

