; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
  (lambda (proc-names vals old-env)
    (recursively-extended-env-record proc-names vals old-env)))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      (empty-env-record () (fail sym))

      (extended-env-record (syms vals env)
	     (let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (succeed (list-ref vals pos))
	      (apply-env env sym succeed fail))))

      (recursively-extended-env-record (procnames vals old-env)
        (let ([pos (list-find-position sym procnames)])
          (if 
            (number? pos)
              (let  
                (
                  [rec-exp (list-ref vals pos)]
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

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*
     )
     (empty-env)
  )
)

(define global-env init-env)

(define apply-env-error
  (lambda (id)
    (eopl:error 'apply-env "variable not found in enviroment: ~s" id)
  )
)

(define apply-global-env
  (lambda (id)
    (apply-env global-env id identity apply-env-error)
  )
)
