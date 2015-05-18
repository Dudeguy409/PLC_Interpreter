;Ruying Chen
;Andrew Davidson
;Noah Miller

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

;TODO apply-k to this result and put it in cps???? Useless, but whatever
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda 
    (syms vals env k)
      ;TODO make list->vector-cps and change this up
      (apply-k k (extended-env-record syms (list->vector vals) env))
  )
)

(define extend-env-recursively
  (lambda 
    (proc-names vals old-env k)
       ;TODO make list->vector-cps and change this up
      (apply-k k (recursively-extended-env-record proc-names (list->vector vals) old-env))
  )
)


;TODO BROKEN  put in CPS!!!
  (define deref
    (lambda
      (ref) 
        (cases reference ref  
          [norm-ref (v i)  (vector-ref v i) ] 
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

  ; (define apply-env 
  ; (lambda 
  ;   (env sym succeed fail) 
  ;     (deref 
  ;       (apply-env-ref env sym)
  ;     )
  ; )

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

;TODO remove call to parser of put in cps???
(define make-init-env         ; for now, our initial global environment only contains 
  (lambda () 
    (extend-env            ; procedure names.  Recall that an environment associates
     (map parse-exp *prim-proc-names*)   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*
     )
     (empty-env)
     ;TODO fix????
     identity
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
     ;TODO put in CPS???? This is an escape procedure so apply-k would be redundant???
    (eopl:error 'apply-env "variable not found in enviroment: ~s" id)
  )
)
; reassign the environment to global-env
(define extend-global-env
  (lambda
    (syms vals k)
    ;TODO put in CPS
      (set! global-env (extend-env syms vals global-env k))
  )
)

(define apply-global-env
  (lambda (id k)
    (apply-env global-env id apply-k apply-env-error k)
  )
)

;TODO BROKEN  put in CPS!!!
(define apply-global-env-ref
  (lambda (id)
    (apply-env-ref global-env id apply-env-error)
  )
)
