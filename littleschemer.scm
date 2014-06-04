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
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
    ((equal? (car l) s) (cdr l))
    (else (cons (car l)
		(rember s (cdr l)))))))

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
     ((sero? m) n)
     (else (edd1 (o+ n (zub1 m)))))))

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

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else 
      (cons (o+ (car tup1) (car tup2))
	    (tup+
	     (cdr tup1) (cdr tup2)))))))
(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (< (sub1 n) (sub1 m))))))

(define =
  (lambda (n m)
    (cond
     ((> n m) #f)
     ((< n m) #f)
     (else #t))))

(define expt
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (x n (expt n (sub1 m)))))))

(define quotient
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (quotient (o- n m) m))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
		 (rempick (sub1 n)
			  (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((number? (car lat))
	     (no-nums (cdr lat)))
	    (else (cons (car lat)
			(no-nums
			 (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat))
      (cons (car lat)
	    (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eq? (car lat) a)
	(add1 (occur a (cdr lat))))
       (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
	(rember* a (cdr l)))
       (else (cons (car l)
		   (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
		 (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons old
	      (cons new
		    (insertR* new old
			      (cdr l)))))
       (else (cons (car l)
		   (insertR* new old
			     (cdr l))))))
     (else (cons (insertR* new old
			   (car l))
		 (insertR* new old
			   (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
	(add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l))
		(occur* a (cdr l)))))))

(define subst* 
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new
	      (subst* new old (cdr l))))
       (else (cons (car l)
		   (subst* new old
			   (cdr l))))))
     (else
      (cons (subst* new old (car l))
	    (subst* new old (cdr l)))))))


(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new
	      (cons old
		    (insertL* new old
			      (cdr l)))))
       (else (cons (car l)
		   (insertL* new old
			     (cdr l)))))
      (else (cons (insertL* new old
			    (car l))
		  (insertL* new old
			    (cdr l))))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) nil)
     ((atom? (car l))
      (or (eq? (car l) a)
	  (member* a (cdr l))))
     (else (or (member* a (car l))
	       (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
    (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2))
     #f)
    (else (eqlist? s1 s2)))))

;;chapter6

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
	   (numbered?
	    (car (cdr (cdr aexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote +))
      (o+ (value (1st-sub-exp nexp))
	  (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote *))
      (x (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp))))
     (else
      (expt (value (1st-sub-exp nexp))
	    (value (2nd-sub-exp nexp)))))))
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

;chapter7

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) (quote()))
     (else (cons (car lat)
		 (makeset
		  (multirember (car lat)
			       (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
	   (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) 
	 (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) nil)
     (else (or ((member? (car set1) set2) #t)
	       (interesect?
		(cdr set1) set2))))))
(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2)
      (cons (car set1)
	    (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1)
		 (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
		      (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
    ((atom? x) #f)
    ((null? x) #f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f))))

(define first
  (lambda (p)
    (cond
     (else (car p)))))

(define second 
  (lambda (p)
    (cond
     (else (car (cdr p))))))

(define build
  (lambda (a1 a2)
    (cond
     (else (cons a1
		 (cons a2 (quote ())))))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else (cons (revpair (car rel))
		 (revrel (cdr rel)))))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

