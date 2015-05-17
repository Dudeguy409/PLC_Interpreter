;; Exam 3 study
(define apply-continuation
	(lambda (k . list-of-values)
    	(apply k list-of-values)))

(define (comb-cps n k con)
	(fact-cps n (lambda (n-fact) (fact-cps k (lambda (k-fact) (fact-cps (- n k) (lambda (n-minus-k-fact) (apply-continuation con (/ n-fact k-fact n-minus-k-fact)))))))))

(define fact-cps
  (lambda (n k)
  	(if (zero? n)
  		(apply-continuation k 1)
  		(fact-cps (- n 1) (trace-lambda contin (small-fact)	(apply-continuation k (* small-fact n)))))))

(define (fib-cps n k)
	(if (< n 2)
		(apply-continuation k 1)
		(fib-cps (- n 1) (lambda (result) (fib-cps (- n 2) (lambda (result2) (apply-continuation k (+ result result2))))))))

(define (append-cps l1 l2 k)
	(if (null? l1)
		(apply-continuation k l2)
		(append (cdr l1) (lambda (result) (apply-continuation k (cons (car l1) result))))))

(define (up-cps 1st k)
	(if (null? 1st)
		(apply-continuation k '())
		(if (pair? (car 1st))
			(up-cps (cdr 1st) (lambda (result) (append-cps (car 1st) result k)))
			(up-cps (cdr 1st) (lambda (result) (apply-continuation k (cons (car 1st) result)))))))

(define (up 1st)
	(if (null? 1st)
		'()
		(if (pair? (car 1st))
			(append (car 1st) (up (cdr 1st)))
			(cons (car 1st) (up (cdr 1st))))))

