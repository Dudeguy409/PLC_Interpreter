;Ruying Chen
;Andrew Davidson
;Noah Miller

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))
  )
)

(define eval-exp
  (lambda (exp env)
    (cases expression exp
	   [lit-exp (exp) exp]
	   [var-exp (exp) (eval-var exp env)]
	   [let-exp (assignments bodies)
		    (eval-let assignments bodies env)
	   ]
       [letrec-exp
            (proc-names vals letrec-body) (eval-lr-return-last letrec-body (extend-env-recursively proc-names vals env))
       ]
	   [lambda-exp (syms bodies)
		       (eval-lambda syms bodies env)
	   ]
	   [lambda-exp-single (sym bodies)
			      (eval-lambda-single sym bodies env)
	   ]
	   [lambda-exp-improper (needed-syms extra-sym bodies)
				(eval-lambda-improper needed-syms extra-sym bodies env)
	   ]
	   [if-else-exp (test-exp true-exp false-exp)
			(eval-if-else test-exp true-exp false-exp env)
	   ]
	   [while-exp (test-exp bodies)
		      (eval-while test-exp bodies env)
	   ]
	   [set!-exp (sym exp)
	   		(set-ref!
		     	(apply-env-ref
		     		env
			    	(get-var-exp-sym sym)
			    	apply-global-env-ref
			   	)
		     	(eval-exp exp env)
		     )
	   ]
	   [app-exp (exps)
		    (eval-app exps env)
	   ]
	   [define-exp (sym def-exp)
	   		(extend-global-env (list sym) (list (eval-exp def-exp env) ) )
	   ]
	   [else
	    (eopl:error 'eval-exp "evaluator found unknown datatype: ~s" exp)
	   ]
    )
  )
)


(define eval-while
  (lambda (test-exp bodies env)
    (if (eval-exp test-exp env)
	(begin
	  (eval-lr-return-last bodies env)
	  (eval-while test-exp bodies env)
	)
	(void)
    )
  )
)

(define eval-lambda
  (lambda (syms bodies env)
    (closure syms bodies env)
  )
)

(define eval-lambda-single
  (lambda (sym bodies env)
    (closure-single sym bodies env)
  )
)

(define eval-lambda-improper
  (lambda (needed-syms extra-sym bodies env)
    (closure-improper needed-syms extra-sym bodies env)
  )
)

(define eval-if-else
  (lambda (test-exp true-exp false-exp env)
    (if (eval-exp test-exp env)
	(eval-exp true-exp env)
	(eval-exp false-exp env)
    )
  )
)

(define eval-let
  (lambda (assignments bodies env)
    (let ([vars (get-assignment-vars assignments)]
	  [vals (eval-args (get-assignment-vals assignments) env)]
 	 )
      (eval-lr-return-last bodies (extend-env vars vals env))
    )
  )
)

(define eval-lr-return-last
  (lambda (bodies env)
    (if (null? (cdr bodies))
	(eval-exp (car bodies) env)
	(let ([first-exp (eval-exp (car bodies) env)])
	  (eval-lr-return-last (cdr bodies) env)
	)
    )
  )
)

(define eval-app
  (lambda (exps env)
     (let ([op (eval-exp (1st exps) env)]
	   [args (eval-args (cdr exps) env)]
	  )
       (apply-proc op args)
     )
  )
)

(define eval-var
  (lambda (sym env)
    (apply-env-ref env
	       sym
	       apply-global-env
    )
  )
)

; evaluate the list of operands, putting results into a list

(define eval-args 
  (lambda (args env)
    (map (lambda (exp) (eval-exp exp env)) args)
  )
)
