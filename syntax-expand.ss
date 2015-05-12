;Ruying Chen
;Andrew Davidson
;Noah Miller

(define syntax-expand
  (lambda (exp)
    (cases expression exp
	   [var-exp (x) exp]
	   [lit-exp (x) exp]
	   [app-exp (exps) 
		    (syntax-expand-app exps)]
	   [lambda-exp (syms bodies)
		       (syntax-expand-lambda syms bodies)
	   ]
	   [named-let-exp (name assigns bodies)
	   		(let
	   			(
	   				[args (map get-tuple-exp assigns)]
	   				[ids (map get-tuple-id  assigns)]
	   			)
	   				(syntax-expand
	   					(letrec-exp
	   						(list name)
	   						(list (lambda-exp ids bodies))
	   						; (list ids)
	   						; (list bodies)
	   						(list (app-exp (cons (var-exp name) args)))
	   					)
	   				)
	   		)
	   ]
	   [lambda-exp-single (sym bodies)
			      (syntax-expand-lambda-single sym bodies)
	   ]
	   [lambda-exp-improper (need-syms extra-sym bodies)
				(syntax-expand-lambda-improper need-syms extra-sym bodies)
	   ]
	   [if-else-exp (test-exp true-exp false-exp)
			(syntax-expand-if test-exp true-exp false-exp)
	   ]
	   [cond-exp (test-exps list-of-bodies)
		     (syntax-expand-cond test-exps list-of-bodies)
	   ]
	   [case-exp (val list-of-tests list-of-bodies)
		     (syntax-expand-case val list-of-tests list-of-bodies)
	   ]
	   [let-exp (assignments bodies)
		    (syntax-expand-let assignments bodies)
	   ]
	   [let*-exp (assignments bodies)
		     (syntax-expand-let* assignments bodies)
	   ]
	   [begin-exp (bodies)
		      (syntax-expand-begin bodies)
	   ]
	   [and-exp (bodies)
		    (syntax-expand-and bodies)
	   ]
	   [or-exp (bodies)
		   (syntax-expand-or bodies)
	   ]
	   [while-exp (test-exp bodies)
		  (syntax-expand-while test-exp bodies)
	   ]
	   [letrec-exp (proc-names vals letrec-bodies) (syntax-expand-letrec proc-names vals letrec-bodies)

	   ]
	   [else exp]
     )
  )
)

(define (syntax-expand-letrec proc-names vals letrec-bodies)
	(letrec-exp proc-names (map syntax-expand vals) (map syntax-expand letrec-bodies)))

(define syntax-expand-app
  (lambda (exps)
    (app-exp (map syntax-expand exps))
  )
)

(define syntax-expand-lambda
  (lambda (syms bodies)
    (lambda-exp syms (map syntax-expand bodies))
  )
)

(define syntax-expand-lambda-single
  (lambda (sym bodies)
    (lambda-exp-single sym (map syntax-expand bodies))
  )
)

(define syntax-expand-lambda-improper
  (lambda (need-syms extra-sym bodies)
    (lambda-exp-improper need-syms extra-sym
			 (map syntax-expand bodies)
    )
  )
)

(define syntax-expand-if
  (lambda (test-exp true-exp false-exp)
    (if-else-exp (syntax-expand test-exp)
		 (syntax-expand true-exp)
		 (syntax-expand false-exp)
    )
  )
)

(define syntax-expand-cond
  (lambda (test-exps list-of-bodies)
    (cond [(null? test-exps) (parse-exp '(void))]
	  [(else-exp? (cadar test-exps))
	   (syntax-expand (begin-exp (map syntax-expand (car list-of-bodies))))
	  ]
	  [else (if-else-exp (syntax-expand (car test-exps))
			     (syntax-expand (begin-exp (map syntax-expand (car list-of-bodies))))
			     (syntax-expand-cond (cdr test-exps) (cdr list-of-bodies))
		)
	  ]
    )
  )
)

(define syntax-expand-case
  (lambda (val list-of-tests list-of-bodies)
    (cond [(null? list-of-tests) (parse-exp '(void))]
	  [(null? (car list-of-tests))
	   (syntax-expand-case val (cdr list-of-tests) (cdr list-of-bodies))
	  ]
	  [(else-exp? (cadaar list-of-tests))
	   (syntax-expand (begin-exp (map syntax-expand (car list-of-bodies))))
	  ]
	  [else
	   (if-else-exp (syntax-expand (app-exp (list (var-exp 'eqv?) (caar list-of-tests) val)))
			(syntax-expand (begin-exp (map syntax-expand (car list-of-bodies))))
			(syntax-expand-case val (cons (cdar list-of-tests) (cdr list-of-tests)) list-of-bodies)
	   )
	  ]
    )
  )
)
    

(define syntax-expand-begin
  (lambda (bodies)
    (app-exp (list (lambda-exp '() (map syntax-expand bodies))))
  )
)

(define syntax-expand-let
  (lambda (assignments bodies)
     (let ([vars (get-assignment-vars assignments)]
	   [vals (get-assignment-vals assignments)]
	   [expanded-bodies (map syntax-expand bodies)]
	  )
       (app-exp (cons (lambda-exp vars expanded-bodies) vals))
     )
  )
)

(define syntax-expand-let*
  (lambda (assignments bodies)
    (let create-inner-lets ([assignments assignments]) 
      (let ([first-var (list (cadar assignments))]
	    [first-val (list (caddar assignments))]
	    [rest-assignments (cdr assignments)]
	   )
	(if (null? rest-assignments)
	    (app-exp (cons (lambda-exp first-var (map syntax-expand bodies)) first-val))
	    (app-exp (cons (lambda-exp first-var (list (create-inner-lets rest-assignments))) first-val))
        )
      )
    )
  )
)

(define syntax-expand-and
  (lambda (bodies)
    (if (null? (cdr bodies))
	(syntax-expand (car bodies))
	(if-else-exp (syntax-expand (car bodies))
		(syntax-expand-and (cdr bodies))
		(lit-exp #f)
	)
    )
  )
)

(define syntax-expand-or
  (lambda (bodies)
    (if (null? (cdr bodies))
	(syntax-expand (car bodies))
	(if-else-exp (syntax-expand (car bodies))
		     (car bodies)
		     (syntax-expand-or (cdr bodies))
	)
    )
  )
)

(define syntax-expand-while
  (lambda (test-exp bodies)
    (while-exp (syntax-expand test-exp)
	       (map syntax-expand bodies)
    )
  )
)
