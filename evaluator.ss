;Ruying Chen
;Andrew Davidson
;Noah Miller

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env) ls-k)
  )
)

(define eval-exp
  (lambda (exp env k)
    (cases expression exp
	   [lit-exp (exp) (apply-k k exp)]
	   [var-exp (exp) (eval-var exp env k)]
	   ;let should be syntax-expanded
	   ; [let-exp (assignments bodies)
		  ;   (eval-let assignments bodies env)
	   ; ]
	   ;TODO BROKEN fix for cps
       [letrec-exp
            (proc-names vals letrec-bodies) (eval-lr-return-last letrec-bodies (recursively-extended-env-record proc-names (list->vector vals) env) k )
       ]
	   [lambda-exp (syms bodies)
		       (eval-lambda syms bodies env k)
	   ]
	   [lambda-exp-single (sym bodies)
			      (eval-lambda-single sym bodies env k)
	   ]
	   [lambda-exp-improper (needed-syms extra-sym bodies)
				(eval-lambda-improper needed-syms extra-sym bodies env k)
	   ]
	   [if-else-exp (test-exp true-exp false-exp)
			(eval-if-else test-exp true-exp false-exp env k)
	   ]
	   ;TODO BROKEN fix for CPS
	   [while-exp (test-exp bodies)
		      (eval-while test-exp bodies env)
	   ]
	   ;TODO BROKEN fix for CPS
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
		    (eval-app exps env k)
	   ]
	   ;TODO BROKEN fix for CPS
	   [define-exp (sym def-exp)
	   		(extend-global-env (list sym) (list (eval-exp def-exp env) ) k)
	   ]
	    ;TODO do we need to do anything with the k here?
	   [else
	    (eopl:error 'eval-exp "evaluator found unknown datatype: ~s" exp)
	   ]
    )
  )
)

;TODO BROKEN fix for CPS
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
  (lambda (syms bodies env k)
    (apply-k k (closure syms bodies env))
  )
)

(define eval-lambda-single
  (lambda (sym bodies env k)
    (apply-k k (closure-single sym bodies env))
  )
)

(define eval-lambda-improper
  (lambda (needed-syms extra-sym bodies env k)
    (apply-k k (closure-improper needed-syms extra-sym bodies env))
  )
)

(define eval-if-else
  (lambda (test-exp true-exp false-exp env k)
  		(eval-exp test-exp env (test-k true-exp false-exp env k))
 ;    (if (eval-exp test-exp env)
	; (eval-exp true-exp env)
	; (eval-exp false-exp env)
 ;    )
  )
)

; (define eval-let
;   (lambda (assignments bodies env)
;     (let ([vars (get-assignment-vars assignments)]
; 	  [vals (eval-args (get-assignment-vals assignments) env)]
;  	 )
;       (eval-lr-return-last bodies (extend-env vars vals env))
;     )
;   )
; )

;TODO BROKEN fix for CPS
(define eval-lr-return-last
  (lambda (bodies env k)
    (if (null? (cdr bodies))
	(eval-exp (car bodies) env k)
	(eval-exp (car bodies) env (eval-lr-k (cdr bodies) env k ) )
    )
  )
)

(define eval-var
  (lambda (sym env k)
    (apply-env env
	       sym
	       apply-k
	       apply-global-env
	       k
    )
  )
)

;TODO make cups
(define eval-app
  (lambda (exps env k)
  	 (eval-exp (car exps)  env  (eval-args-for-app-k (cdr exps) env k)  )
  )
)

; evaluate the list of operands, putting results into a list

;TODO make CPS
(define eval-args
  (lambda (args env k)
    (map-cps (lambda (exp kont) (eval-exp exp env kont)) args  k  )
  )
)
