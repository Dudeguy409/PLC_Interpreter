;Ruying Chen
;Andrew Davidson
;Noah Miller

;; Parsed expression datatypes

(define-datatype expression exp?
  [var-exp (datum symbol?)]
  [lit-exp (datum always?)]
  [let-exp (assigns (list-of assignment?)) (bodies (list-of exp?))]
  [named-let-exp (name exp?) (assigns (list-of assignment?)) (bodys (list-of exp?))]
  [let*-exp (assigns (list-of assignment?)) (bodies (list-of exp?))]
  [letrec-exp (proc-names (list-of exp?))  (vals (list-of exp?)) (letrec-bodies (list-of exp?))]
  [lambda-exp (syms (list-of exp?)) (bodies (list-of exp?))]
  [lambda-exp-single (sym exp?) (bodies (list-of exp?))]
  [lambda-exp-improper (need-syms (list-of exp?)) (extra-sym exp?) (bodies (list-of exp?))]
  [if-else-exp (test exp?) (true exp?) (false exp?)]
  [begin-exp (bodies (list-of exp?))]
  [and-exp (bodies (list-of exp?))]
  [or-exp (bodies (list-of exp?))]
  [cond-exp (test-exps (list-of exp?)) (list-of-bodies (list-of (list-of exp?)))]
  [case-exp (exp exp?) (list-of-tests (list-of (list-of exp?))) (list-of-bodies (list-of (list-of exp?)))]
  [while-exp (test-exp exp?) (bodies (list-of exp?))]
  [set!-exp (sym exp?) (exp exp?)]
  [define-exp (sym exp?) (exp exp?)]
  [app-exp (exps (list-of exp?))]
)

(define-datatype assignment assignment?
  [assign (sym exp?) (value exp?)]
)
	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

	 
;; environment type definitions




;vector used to have type checks : list-of always?  and list-of exp?
(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
    (syms (list-of exp?))
    (v vector?)
    (env environment?)
  ]
  [recursively-extended-env-record
    (proc-names (list-of exp?))
    (v vector?)
    (env environment?)
  ]
)

(define-datatype continuation continuation?
  [test-k
    (then-exp exp?)
    (else-exp exp?)
    (env environment?)
    (k continuation?)
  ]
  [identity-k]
  [list-k]
  [eval-args-for-app-k 
    (args (list-of exp?)) 
    (env environment?) 
    (orig-k continuation?) 
  ]
  [extend-global-env-k
    (syms (list-of exp?))
    (kont continuation?)
  ]
  [extend-improper-closure-k
    (bodies (list-of exp?))
    (extra-sym exp?)
    (needed-syms (list-of exp?))
    (env environment?)
    (k continuation?)
  ]
  [eval-lr-k
    (bodies (list-of exp?))
    (env  environment?)
    (k continuation?)
  ]
  [map-k
    (proc-cps procedure?)
    (ls list?)
    (k continuation?)
  ]
  [map-combine-k
    (rslt scheme-value?)
    (kont continuation?)
  ]
  [apply-proc-k
    (rslt proc-val?)
    (orig-k continuation?)
  ]
  [split-list-k
    (val scheme-value?)
    (k continuation?)
  ]
  [apply-env-k
    (k continuation?)
    (v vector?)
    (succeed procedure?)
    (fail procedure?)
    (old-env environment?)
    (sym symbol?)
  ]
  [apply-env-ref-k
    (k continuation?)
    (v vector?)
    (fail procedure?)
    (old-env environment?)
    (sym symbol?)
  ]
  [list-index-k
    (k continuation?)
  ]
  [apply-env-recur-k
    (k continuation?)
    (v vector?)
    (succeed procedure?)
    (fail procedure?)
    (old-env environment?)
    (sym symbol?)
    (new-env environment?)
  ]
  [apply-env-recur-ref-k
    (k continuation?)
    (v vector?)
    (fail procedure?)
    (old-env environment?)
    (sym symbol?)
    (new-env environment?)
  ]
  [set-ref!-k
    (sym symbol?)
    (env environment?)
    (k continuation?)
  ]
  [second-set-ref!-k
    (k continuation?)
    (rslt scheme-value?)
  ]
)

(define id-k (identity-k) )
(define ls-k (list-k) )



(define-datatype proc-val proc-val?
  [prim-proc (name symbol?)]
  [closure
    (vars (list-of exp?))
    (bodies (list-of exp?))
    (env environment?)
  ]
  [closure-single 
    (var exp?)
    (bodies (list-of exp?))
    (env environment?)
  ]
  [closure-improper 
    (needed-syms (list-of exp?))
    (extra-sym exp?)
    (bodies (list-of exp?))
    (env environment?)
  ]
  [continuation-proc
    (k continuation?)
  ]
)

(define-datatype reference reference?
  [norm-ref (v vector?) (i number?)]
)

