;Ruying Chen
;Andrew Davidson
;Noah Miller

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector (get-extend-env-vals syms vals)) env)))

(define get-extend-env-vals
  (lambda
    (syms vals)
      (if
        (null? syms)
          '()
          (if
            (is-ref-exp? (car syms))
              (cons (car vals) (get-extend-env-vals (cdr syms) (cdr vals) ) )
              (if (reference? (car vals))
                (cons (deref (car vals)) (get-extend-env-vals (cdr syms) (cdr vals) ) )
                (cons (car vals) (get-extend-env-vals (cdr syms) (cdr vals) ) )
              )
          )
      )
  )
)

(define extend-env-recursively
  (lambda (proc-names vals old-env)
    (recursively-extended-env-record proc-names (list->vector (get-extend-env-vals proc-names vals)) old-env)))



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
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record () (fail sym))
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
     (map parse-exp *prim-proc-names*)   ;  a value (not an expression) with an identifier.
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
      (set! global-env (extend-env syms vals global-env))
  )
)

(define apply-global-env
  (lambda (id)
    (apply-env global-env id identity apply-env-error)
  )
)

(define apply-global-env-ref
  (lambda (id)
    (apply-env-ref global-env id apply-env-error)
  )
)

(define deref-if-necessary (lambda (x) (if (reference? x) (deref x) x ) ) )
