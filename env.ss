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

;TODO BROKEN  put in CPS!!!
  (define set-ref!
    (lambda
      (ref val)
      (cases reference ref
        [norm-ref (v i)  (vector-set! v i val) ]
      )
    )
  )

;TODO BROKEN  put in CPS!!!
  (define apply-env-ref
    (lambda
      (env sym fail)
        (cases environment env
          [extended-env-record (syms v old-env)
            (let
              (
                (pos (list-find-position sym syms))
              )
                (if
                  (number? pos)
                    (norm-ref v pos)
                    (apply-env-ref old-env sym fail)
                )
            )
          ]
          [empty-env-record () (fail sym)]
          [recursively-extended-env-record (procnames v old-env)
            (let 
              (
                [pos (list-find-position sym procnames)]
              )
                (if
                  (number? pos)
                    (norm-ref v pos)
                    (apply-env-ref old-env sym fail)
                )
            )
          ]
        )
    )
  )

(define apply-env
  (lambda (env sym succeed fail k) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      [empty-env-record () (fail sym k)]
      ;TODO make cps
      [extended-env-record (syms v old-env)
	     (let ((pos (list-find-position sym syms)))
      	  (if
            (number? pos)
	           (succeed k (vector-ref v pos))
	           (apply-env old-env sym succeed fail k))
       )
      ]
      ;TODO BROKEN  put in CPS!!!
      [recursively-extended-env-record (procnames v old-env)
        (let ([pos (list-find-position sym procnames)])
          (if 
            (number? pos)
              (let  
                (
                  [rec-exp (vector-ref v pos)]
                ) 
                  (cases expression rec-exp 
                    [lambda-exp (syms bodies) (closure syms bodies env)]
                    [lambda-exp-improper (needed-syms extra-sym bodies) (closure-improper needed-syms extra-sym bodies env) ]
                    [else (eopl:error 'apply-env "tried to store something besides a lambda in letrec:~s" rec-exp)]
                  ) 
              )
              (apply-env old-env sym succeed fail)
          )
        )
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

;TODO fix
(define make-init-env         ; for now, our initial global environment only contains 
  (lambda ()
    (extended-env-record           ; procedure names.  Recall that an environment associates
    (map-cps make-prim-proc-var-exp-cps *prim-proc-names* id-k)   ;  a value (not an expression) with an identifier.
      (list->vector
        (map prim-proc *prim-proc-names*)
      )
      (empty-env)
    )
  )
)

(define global-env (make-init-env) )



;TODO put in CPS
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
    ;TODO put in CPS
      (set! global-env (extended-env-record syms (list->vector vals) global-env))
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
