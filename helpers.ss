;Ruying Chen
;Andrew Davidson
;Noah Miller

(define identity
  (lambda (x)
    x
  )
)

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define scheme-value? (lambda (x) #t) )

;TODO THESE ARE BAD AND SHOULD BE DELETED????
(define assign-sym car)
(define assign-exp cadr)

(define named-let-name cadr)
(define named-let-pairs caddr)
(define named-let-bodies cdddr)

(define let-pairs cadr)
(define let-bodies cddr)

(define lambda-vars cadr)
(define lambda-bodies cddr)

(define if-test cadr)
(define if-true caddr)
(define if-false cadddr)

(define while-test cadr)
(define while-bodies cddr)

(define if-exp?
  (lambda (exp)
    (eqv? (car exp) 'if)
  )
)

(define cond-exp?
  (lambda (exp)
    (eqv? (car exp) 'cond)
  )
)

(define else-exp?
  (lambda (exp)
    (eqv? exp 'else)
  )
)

(define case-exp?
  (lambda (exp)
    (eqv? (car exp) 'case)
  )
)

(define set!-exp?
  (lambda (exp)
    (eqv? (car exp) 'set!)
  )
)

(define lambda-exp? 
  (lambda (exp)
    (eqv? (car exp) 'lambda)
  )
)


(define let-exp? 
  (lambda (exp)
    (eqv? (car exp) 'let)
  )
)


(define let*-exp? 
  (lambda (exp)
    (eqv? (car exp) 'let*)
  )
)


(define letrec-exp? 
  (lambda (exp)
    (eqv? (car exp) 'letrec)
  )
)

(define quote? 
  (lambda (x) 
    (eqv? (car x) 'quote)
  )
)

(define begin?
  (lambda (x)
    (eqv? 'begin (car x))
  )
)

(define and?
  (lambda (x)
    (eqv? 'and (car x))
  )
)

(define or?
  (lambda (x)
    (eqv? 'or (car x))
  )
)

(define while?
  (lambda (x)
    (eqv? 'while (car x))
  )
)

(define lit-exp?
  (lambda (exp)
    (cond [(symbol? exp) #t]
	  [(number? exp) #t]
	  [(string? exp) #t]
	  [(boolean? exp) #t]
	  [(char? exp) #t]
	  [(vector? exp) #t]
	  [(not (pair? exp)) #f]
	  [else (quote? exp)]
    )
  )
)




;=========================================================================================================
;=============   NEED TO BE IN CPS ==================================================


; any procedure that map-cps takes as its first argument must be in CPS form.
(define map-cps
  (lambda
    (proc-cps ls k)
      (cond
        [(null? ls)
          (apply-k k '())
        ]
        [else
          (proc-cps
            (car ls)
              (map-k proc-cps (cdr ls)  k)
          )
        ]
      )
  )
)

;TODO THESE ARE BAD AND SHOULD BE DELETED????
(define get-assignment-vars
  (lambda (assignments)
    (map 2nd assignments)
  )
)

;TODO THESE ARE BAD AND SHOULD BE DELETED????
(define get-assignment-vals
  (lambda (assignments)
    (map 3rd assignments)
  )
)

(define improper-list-start
  (lambda (lst)
    (if (pair? lst)
	(cons (car lst) (improper-list-start (cdr lst)))
	'()
    )
  )
)

(define improper-list-end
  (lambda (lst)
    (if (pair? lst)
	(improper-list-end (cdr lst))
	lst
    )
  )
)

(define check-improper-list
  (lambda (sym-list proc?)
    (if (list? sym-list)
	#f ; proper list
	(let loop ([sym-list sym-list])
	  (cond [(proc? sym-list) #t] ; end of improper list
		[(not (proc? (car sym-list))) #f]; bad list
		[else (loop (cdr sym-list))]
	  )
        )
    )
  )
)

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (equal? sym (get-var-exp-sym xsym))) los)
  )
)

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f
	     )
	   )
     )
    )
  )
)

(define split-list
  (lambda (lis num)
    (let split-list ([lis lis] [num num] [beginning '()])
      (if (= num 0)
	  (list (reverse beginning) lis)
	  (split-list (cdr lis) (- num 1) (cons (car lis) beginning))
      )
    )
  )
)
