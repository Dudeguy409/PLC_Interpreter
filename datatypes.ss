;Ruying Chen
;Andrew Davidson
;Noah Miller

;; Parsed expression datatypes

(define-datatype expression exp?
  (var-exp (datum symbol?))
  (lit-exp (datum always?))
  (let-exp (assigns (list-of assignment?)) (bodies (list-of exp?)))
  (named-let-exp (name symbol?) (assigns (list-of assignment?)) (bodys (list-of exp?)))
  (let*-exp (assigns (list-of assignment?)) (bodies (list-of exp?)))
  (letrec-exp (proc-names (list-of symbol?))  (vals (list-of exp?)) (letrec-bodies (list-of exp?)))
  (lambda-exp (syms (list-of symbol?)) (bodies (list-of exp?)))
  (lambda-exp-single (sym symbol?) (bodies (list-of exp?)))
  (lambda-exp-improper (need-syms (list-of symbol?)) (extra-sym symbol?) (bodies (list-of exp?)))
  (if-else-exp (test exp?) (true exp?) (false exp?))
  (begin-exp (bodies (list-of exp?)))
  (and-exp (bodies (list-of exp?)))
  (or-exp (bodies (list-of exp?)))
  (cond-exp (test-exps (list-of exp?)) (list-of-bodies (list-of (list-of exp?))))
  (case-exp (exp exp?) (list-of-tests (list-of (list-of exp?))) (list-of-bodies (list-of (list-of exp?))))
  (while-exp (test-exp exp?) (bodies (list-of exp?)))
  (set!-exp (sym symbol?) (exp exp?))
  (app-exp (exps (list-of exp?)))
)

(define-datatype assignment assignment?
  (assign (sym symbol?) (value exp?))
)
	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

	 
;; environment type definitions

;vector used to have type checks : list-of always?  and list-of exp?
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (v vector?)
   (env environment?)
  )
  [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (v vector?)
    (env environment?)]
)



(define-datatype proc-val proc-val?
  (prim-proc (name symbol?))
  (closure (vars (list-of symbol?)) 
	   (bodies (list-of exp?)) 
	   (env environment?)
  )
  (closure-single (var symbol?)
		  (bodies (list-of exp?))
		  (env environment?)
  )
  (closure-improper (needed-syms (list-of symbol?))
		    (extra-sym symbol?)
		    (bodies (list-of exp?))
		    (env environment?)
  )
)

(define-datatype reference reference? [norm-ref (v vector?) (i number?) ] )

(define get-tuple-id (lambda (tup) (cases assignment tup [assign (id exp) id])))
(define get-tuple-exp (lambda (tup) (cases assignment tup [assign (id exp) exp])))
