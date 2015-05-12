;Ruying Chen
;Andrew Davidson
;Noah Miller

(define replace-procs
  (lambda (ls)
    (cond [(proc-val? ls) '<interpreter_procedure>]
	  [(list? ls) (map replace-procs ls)]
	  [else ls]
    )
  )
)

(define interpret-input
  (lambda (input)
    (replace-procs (top-level-eval (syntax-expand (parse-exp input))))
  )
)

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    (let ([read-exp (read)])
      (if (not (equal? read-exp '(exit)))
	  (begin (eopl:pretty-print (interpret-input read-exp)) 
		 (newline)
		 (rep)
	  )
      )
    )
  )
)

(define eval-one-exp
  (lambda (x) (interpret-input x))
)
