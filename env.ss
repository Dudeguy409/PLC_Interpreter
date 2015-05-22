;Ruying Chen
;Andrew Davidson
;Noah Miller

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

  (define deref
    (lambda
      (ref k) 
        (cases reference ref  
          [norm-ref (v i)  
            (apply-k k (vector-ref v i)) 
          ] 
        )  
    ) 
  )

    ;TODO this will call apply-k with void args!!!  Is this a problem?
  (define set-ref!
    (lambda
      (ref val k)
      (cases reference ref
        [norm-ref (v i)  (apply-k k (vector-set! v i val)) ]
      )
    )
  )

  (define apply-env-ref
    (lambda
      (env sym fail k)
        (cases environment env
          [extended-env-record (syms v old-env)
            (list-find-position-cps sym syms (apply-env-ref-k k v fail old-env sym) )
          ]
          [empty-env-record () (fail sym)]
          [recursively-extended-env-record (procnames v old-env)
            (list-find-position-cps sym procnames (apply-env-recur-ref-k k v fail old-env sym env) )
          ]
        )
    )
  )

(define apply-env
  (lambda (env sym succeed fail k) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      [empty-env-record () 
        (fail sym k)
      ]
      [extended-env-record (syms v old-env)
        (list-find-position-cps sym syms (apply-env-k k v succeed fail old-env sym) )
      ]
      [recursively-extended-env-record (procnames v old-env)
        (list-find-position-cps sym procnames (apply-env-recur-k k v succeed fail old-env sym env) )
      ]
    )
  )
)

(define make-prim-proc-var-exp-cps
  (lambda
    (proc-sym k)
      (apply-k k (var-exp proc-sym))
  )
)

(define make-init-env         ; for now, our initial global environment only contains 
  (lambda ()
    (extended-env-record          ; procedure names.  Recall that an environment associates
    (map-cps make-prim-proc-var-exp-cps *prim-proc-names* id-k)   ;  a value (not an expression) with an identifier.
      (list->vector
        (map prim-proc *prim-proc-names*)
      )
      (empty-env-record)
    )
  )
)

(define global-env (make-init-env) )

(define reset-global-env 
  (lambda 
    () 
      (set! global-env (make-init-env))
  )
)

(define apply-env-error
  (lambda (id k)
    (eopl:error 'apply-env "variable not found in enviroment: ~s" id)
  )
)
; reassign the environment to global-env
(define extend-global-env
  (lambda
    (syms vals k)
    ;TODO this will call apply-k with void args!!!  Is this a problem?
      (apply-k k (set! global-env (extended-env-record syms (list->vector vals) global-env)))
  )
)

(define apply-global-env
  (lambda (id k)
    (apply-env global-env id apply-k apply-env-error k)
  )
)

(define apply-global-env-ref
  (lambda (id k)
    (apply-env-ref global-env id apply-env-error k)
  )
)
