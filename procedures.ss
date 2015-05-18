;Ruying Chen
;Andrew Davidson
;Noah Miller

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      ;TODO put into cps!!!!
      [prim-proc (op) (apply-prim-proc op args k)]
       ;TODO put into cps!!!!
      [closure (vars bodies env)
	       (apply-closure vars args bodies env k)
      ]
       ;TODO put into cps!!!!
      [closure-single (sym bodies env)
		      (apply-closure-single sym args bodies env k)
      ]
       ;TODO put into cps!!!!
      [closure-improper (needed-syms extra-sym bodies env)
			(apply-closure-improper needed-syms extra-sym args bodies env k)
      ]
      ;TODO do we need to do anything with the k here?
      [else 
        (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value
	      )
      ]
    )
  )
)

(define apply-k
  (lambda (k val)
    (cases continuation k
      [test-k (then-exp else-exp env kont)
        (if val
            (eval-exp then-exp env kont)
            (eval-exp else-exp env kont))]
      [rator-k (rands env kont)
        (eval-rands rands 
                    env
                    (rands-k val kont))]
      [rands-k (proc-value kont)
        (apply-proc proc-value val kont)]
      [identity-k () val]
      [list-k () (list val) ]
      [eval-lr-k (bodies env kont) (eval-lr-return-last bodies env kont) ]
      [eval-args-for-app-k (args env orig-k)  (eval-args args env (apply-proc-k val orig-k)  ) ]
      [map-k (proc-cps ls kont) (map-cps proc-cps ls (map-combine-k val kont))]
      [map-combine-k (rslt kont)  (apply-k kont (cons rslt val) ) ]
      [apply-proc-k (rslt orig-k) (apply-proc rslt val orig-k) ]
      )
  )
)

(define apply-closure
  (lambda (vars args bodies env k)
    ;TODO put length in CPS????
    (if (= (length vars) (length args))
	(eval-lr-return-last bodies (extended-env-record vars (list->vector args) env) k)
	(eopl:error 'apply-proc "invalid number of arguments ~s" args)
    )
  )
)

(define apply-closure-single
  (lambda (sym args bodies env k)
    (let ([syms (list sym)]
	  [args-list (list args)]
	 )
      (eval-lr-return-last bodies
			   (extended-env-record syms (list->vector args-list) env)
         k
      )
    )
  )
)

(define apply-closure-improper
  (lambda (needed-syms extra-sym args bodies env k)
    (let ([num-needed-syms (length needed-syms)])
      (if (<= num-needed-syms (length args))
	  (let* ([splits (split-list args num-needed-syms)]
		 [new-env (extended-env-record 
			   (cons extra-sym needed-syms)
			   (list->vector (cons (cadr splits) (car splits)))
			   env
			  )
		 ]
		)
	    (eval-lr-return-last bodies new-env)
	  )
	  (eopl:error 'apply proc "too few arguments ~s" args)
       )
    )
  )
)

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define *prim-proc-names*
  '(+ - * / add1 sub1 zero? not = < > <= >= cons
      car  cdr caar cddr cadr cdar caaar cdddr caadr cddar cadar cdadr cdaar caddr
      list null? assq eq? eqv? equal? atom? length list->vector list? pair? procedure? 
      vector->list vector make-vector vector-ref vector? number? symbol? set-car! 
      set-cdr! vector-set! display newline map list-tail apply void quotient remainder odd? even? append display newline
   )
)

;TODO ask claude if these are primitive procs
 ;TODO split this into prim and non-prim???  MAKE CPS
(define (apply-prim-proc prim-proc args k)
  (apply-k k (case prim-proc
    [(+) (apply + args)]
    [(-) (apply - args)]
    [(*) (apply * args)]
    [(/) (apply / args)]
    [(add1) (+ (car args) 1)]
    [(sub1) (- (car args) 1)]
    [(quotient) (apply quotient args)]
    [(remainder) (apply remainder args)]

    [(zero?) (= (car args) 0)]
    [(not) (apply not args)]    
    [(=) (apply = args)]
    [(<) (apply < args)]
    [(>) (apply > args)]
    [(<=) (apply <= args)]
    [(>=) (apply >= args)]
    
    [(cons) (apply cons args)]

    [(car) (apply car args)] 
    [(cdr) (apply cdr args)] 
    [(caar) (apply caar args)] 
    [(cddr) (apply cddr args)] 
    [(cadr) (apply cadr args)] 
    [(cdar) (apply cdar args)] 
    [(caaar) (apply caaar args)] 
    [(cdddr) (apply cdddr args)] 
    [(caadr) (apply caadr args)] 
    [(cddar) (apply cddar args)] 
    [(cadar) (apply cadar args)] 
    [(cdadr) (apply cdadr args)] 
    [(cdaar) (apply cdaar args)] 
    [(caddr) (apply caddr args)]
    [(append) (apply (lambda ls (append (1st args) ls)) (2nd args))]
    [(odd?) (odd? (1st args))]
    [(even?) (even? (1st args))]
    [(list-tail) (list-tail (1st args) (2nd args))]
    [(list) args]
    [(null?) (apply null? args)]
    [(assq) (apply assq args)]
    [(eq?) (apply eq? args)]
    [(eqv?) (apply eqv? args)]
    [(equal?) (apply equal? args)]
    [(atom?) (apply atom? args)]
    [(length) (apply length args)]
    [(list->vector) (apply list->vector args)]
    [(list?) (apply list? args)]
    [(pair?) (apply pair? args)]
    [(procedure?) (apply proc-val? args)]
    [(vector->list) (apply vector->list args)]
    [(vector) (apply vector args)]
    [(newline) (newline)]
    [(display) (display (1st args))]
    [(make-vector) (apply make-vector args)]
    [(vector-ref) (apply vector-ref args)]
    [(vector?) (apply vector? args)]
    [(number?) (apply number? args)]
    [(symbol?) (apply symbol? args)]

    [(set-car!) (apply set-car! args)]
    [(set-cdr!) (apply set-cdr! args)]
    [(vector-set!) (apply vector-set! args)]

    [(display) (apply display args)]
    [(newline) (apply newline args)]
    
    [(map) (map (lambda (x) (apply-proc (1st args) (list x))) (2nd args))]
    [(apply) (apply-proc (1st args) (2nd args) id-k)]
    [(void) (void)]
    [else (error 'apply-prim-proc 
                 "Bad primitive procedure name: ~s" 
                 prim-op
	  )
    ]
  ))
)
