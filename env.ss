;Ruying Chen
;Andrew Davidson
;Noah Miller

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define extend-env-recursively
  (lambda (proc-names vals old-env)
    (recursively-extended-env-record proc-names (list->vector vals) old-env)))



  (define deref 
    (lambda 
      (ref) 
        (cases reference ref  
          [norm-ref (v i)  (vector-ref v i) ] 
        )  
    ) 
  )

  (define set-ref! 
    (lambda 
      (ref val) 
      (cases reference ref  
        [norm-ref (v i)  (vector-set! v i val) ] 
      ) 
    ) 
  )

  ; (define apply-env 
  ; (lambda 
  ;   (env sym succeed fail) 
  ;     (deref 
  ;       (apply-env-ref env sym)
  ;     )
  ; )

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
          [else (eopl:error 'apply-env-ref "You tried to use a set! in letrec, tool:~s" sym) ] 
        ) 
    ) 
  )

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record () (fail sym))
;TODO add vectors to all environments and then....
      (extended-env-record (syms v old-env)
	     (let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (vector-ref v pos))
	      (apply-env old-env sym succeed fail))))

      (recursively-extended-env-record (procnames v old-env)
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
      )
    )
  )
)

(define make-init-env         ; for now, our initial global environment only contains 
  (lambda () 
    (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*
     )
     (empty-env)
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
  (lambda (id)
    (eopl:error 'apply-env "variable not found in enviroment: ~s" id)
  )
)
; reassign the environment to global-env
(define extend-global-env
  (lambda
    (syms vals)
      (set! global-env (extend-env syms vars global-env))
  )
)

(define apply-global-env
  (lambda (id)
    (apply-env global-env id identity apply-env-error)
  )
)
