(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)] ; symbols
     [(lit-exp? datum) (parse-lit datum)] ; all semi-primative types
     [(pair? datum) ; all list types
      (cond
       [(lambda-exp? datum) ; lambdas here
	(parse-lambda datum)
       ]
       [(let-exp? datum) ; let type
	(parse-let datum)
       ]
       [(let*-exp? datum) ; let*
	(parse-let* datum)
       ]
       [(letrec-exp? datum) ; letrec
	(parse-letrec datum)
       ]
       [(if-exp? datum) ; if
	(parse-if datum)
       ]
       [(cond-exp? datum)
	(parse-cond datum)
       ]
       [(case-exp? datum)
	(parse-case datum)
       ]
       [(begin? datum)
	(parse-begin datum)
       ]
       [(and? datum)
	(parse-and datum)
       ]
       [(or? datum)
	(parse-or datum)
       ]
       [(while? datum)
	(parse-while datum)
       ]
       [(set!-exp? datum) ; set!
	(parse-set! datum)
       ]
       [(check-improper-list datum always?) 
	(eopl:error 'parse-exp "found an improper list: ~s" datum)
       ]
       [else (app-exp (map parse-exp datum))] ; app-exp
      )
     ]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define parse-lit
  (lambda (exp)
    (if (pair? exp)
	(lit-exp (2nd exp))
	(lit-exp exp)
    )
  )
)

(define parse-set!
  (lambda (exp)
    (cond [(null? (cdr exp))
	   (eopl:error 'parse-exp "found an empty set! expression: ~s" exp)
	  ]
	  [(null? (cddr exp))
	   (eopl:error 'parse-exp "found a set! without an assignment: ~s" exp)
	  ]
	  [(not (null? (cdddr exp)))
	   (eopl:error 'parse-exp "found a set! with too many arguments: ~s" exp)
	  ]
	  [(not (symbol? (cadr exp)))
	   (eopl:error 'parse-exp "found a set! with non-symbol as the first argument: ~s" exp)
	  ]
	  [else
	   (set!-exp (cadr exp) (parse-exp (caddr exp)))
	  ]
    )
  )
)

(define parse-begin
  (lambda (exp)
    (cond [(null? (cdr exp))
	   (eopl:error 'parse-exp "found an empty begin: ~s" exp)
	  ]
	  [else 
	   (begin-exp (map parse-exp (cdr exp)))
	  ]
    )
  )
)

(define parse-if
  (lambda (exp)
    (cond [(null? (cdr exp))
	   (eopl:error 'parse-exp "found an empty if: ~s" exp)
	  ]
	  [(null? (cddr exp))
	   (eopl:error 'parse-exp "found an if without a true expression: ~s" exp)
	  ]
	  [(null? (cdddr exp))
	   (if-else-exp (parse-exp (if-test exp)) 
			(parse-exp (if-true exp))
			(app-exp (list (var-exp 'void)))
	   )
	  ]
	  [(not (null? (cddddr exp)))
	   (eopl:error 'parse-exp "found an if with too many arguments: ~s" exp)
	  ]
	  [else
	   (if-else-exp (parse-exp (if-test exp))
			(parse-exp (if-true exp))
			(parse-exp (if-false exp))
	   )
	  ]
    )
  )
)

(define parse-cond
  (lambda (exp)
    (cond [(null? (cdr exp))
	   (eopl:error 'parse-exp "Found empty cond expression: ~s" exp)
	  ]
	  [(andmap (lambda (x) (not (pair? x))) (cdr exp))
	   (eopl:error 'parse-exp "Found empty cond expression: ~s" exp)
	  ]
	  [(ormap null? (map cdr (cdr exp)))
	   (eopl:error 'parse-exp "Found empty cond expression: ~s" exp)
	  ]
	  [else 
	   (let ([test-exps (map car (cdr exp))]
		 [list-of-bodies (map 
				  (lambda (x) (map parse-exp x)) 
				  (map cdr (cdr exp))
				 )
		 ]
		)
	     (if (= (or (list-index else-exp? test-exps) (- (length list-of-bodies) 1)) (- (length list-of-bodies) 1))
		 (cond-exp (map parse-exp test-exps) list-of-bodies)
		 (eopl:error 'parse-exp "Found an else not at the end of a cond expression: ~s" exp)
	     )
	   )  
	  ]
    )
  )
)
	  
(define parse-case
  (lambda (exp)
    (cond [(null? (cdr exp))
	   (eopl:error "Found an empty case expression: ~s" exp)
	  ]
	  [(null? (cddr exp))
	   (eopl:error "Found an empty case expression: ~s" exp)
	  ]
	  [(andmap (lambda (x) (not (pair? x))) (cddr exp))
	   (eopl:error 'parse-exp "Found empty case expression: ~s" exp)
	  ]
	  [(ormap null? (map cdr (cddr exp)))
	   (eopl:error 'parse-exp "Found empty case expression: ~s" exp)
	  ]
	  [else
	   (let ([test-exps (map car (cddr exp))]
		 [list-of-bodies (map 
				  (lambda (x) (map parse-exp x)) 
				  (map cdr (cddr exp))
				 )
		 ]
		)
	     (if (= (or (list-index else-exp? test-exps) (- (length list-of-bodies) 1)) (- (length list-of-bodies) 1))
		 (case-exp (parse-exp (cadr exp)) (literate-tests test-exps) list-of-bodies)
		 (eopl:error 'parse-exp "Found an else not at the end of a case expression: ~s" exp)
	     )
	   )
	  ]
    )
  )
)     

(define literate-tests
  (lambda (test-exps)
    (if (else-exp? (car test-exps))
	(list (list (lit-exp 'else)))
	(cons (map (lambda (y) (lit-exp y)) (car test-exps)) (literate-tests (cdr test-exps)))
    )
  )
)


(define get-lambda-ids (lambda (exp) (cases expression exp [lambda-exp (syms bodies) syms][else (eopl:error 'parse-exp "letrecs can only contain lambda expressions: ~s" exp)] )))
(define get-lambda-bodies (lambda (exp) (cases expression exp [lambda-exp (syms bodies) bodies][else (eopl:error 'parse-exp "letrecs can only contain lambda expressions: ~s" exp)])))

(define parse-letrec
   (lambda (exp)
    (cond [(null? (cdr exp)) ; let with no variables and body
	   (eopl:error 'parse-exp "found an empty letrec: ~s" exp)
	  ]
	  [(null? (let-bodies exp)) ; let with no bodies
	   (eopl:error 'parse-exp "found letrec with no bodies: ~s" exp)
	  ]
	  [(check-improper-list (let-pairs exp) always?)
	   (eopl:error 'parse-exp "found an improper list in the argument section of letrec: ~s" exp)
	  ]
	  [else ; should be good
	  	(let 
	  		(
	  			[tuples (map parse-assignment (let-pairs exp)) ]
	  			[letrec-bodies  (map parse-exp (let-bodies exp))]
	  		) 
	  		(let  
	  			(
	  				[proc-names (map get-tuple-id tuples)][vals (map get-tuple-exp tuples)])  
	  				(let  ([ids (map get-lambda-ids vals) ][bodies (map get-lambda-bodies vals) ]) 
	  					(letrec-exp proc-names ids bodies letrec-bodies) 
	  				) 
	  		) 
	  	)
	  ]
    )
  )
)



(define parse-let*
 (lambda (exp)
    (cond [(null? (cdr exp)) ; let with no variables and body
	   (eopl:error 'parse-exp "found an empty let*: ~s" exp)
	  ]
	  [(null? (let-bodies exp)) ; let with no bodies
	   (eopl:error 'parse-exp "found let* with no bodies: ~s" exp)
	  ]
	  [(check-improper-list (let-pairs exp) always?)
	   (eopl:error 'parse-exp "found an improper list in the argument section of let*: ~s" exp)
	  ]
	  [else ; should be good
	   (let*-exp 
	    (map parse-assignment (let-pairs exp)) 
	    (map parse-exp (let-bodies exp))
	   )
	  ]
    )
  )
)
(define parse-let
  (lambda (exp)
    (cond [(null? (cdr exp)) ; let with no variables and body
	   (eopl:error 'parse-exp "found an empty let: ~s" exp)
	  ]
	  [(symbol? (named-let-name exp)) ; named let
	   (parse-named-let exp)
	  ]
	  [(null? (let-bodies exp)) ; let with no bodies
	   (eopl:error 'parse-exp "found let with no bodies: ~s" exp)
	  ]
	  [(check-improper-list (let-pairs exp) always?)
	   (eopl:error 'parse-exp "found an improper list in the argument section of let: ~s" exp)
	  ]
	  [else ; should be good
	   (let-exp 
	    (map parse-assignment (let-pairs exp)) 
	    (map parse-exp (let-bodies exp))
	   )
	  ]
    )
  )
)

(define parse-named-let
  (lambda (exp)
    (cond [(null? (cddr exp)) ; named let without an variables
	   (eopl:error 'parse-exp "found a named let with only a name: ~s" exp)
	  ]
	  [(null? (named-let-bodies exp)) ; named let without any bodiess
	   (eopl:error 'parse-exp "found a named let without any bodies: ~s" exp)
	  ]
	  [(check-improper-list (named-let-pairs exp) always?)
	   (eopl:error 'parse-exp "found an improper list in the argument section of named let: ~s" exp)
	  ]
	  [(named-let-exp ; should be fine
	  	(cadr exp)
	    (map parse-assignment (named-let-pairs exp))
	    (map parse-exp (named-let-bodies exp))
	   )
	  ]
    )
  )
)

(define parse-assignment
  (lambda (exp)
    (cond [(not (pair? exp)) ; needs to be a list
	   (eopl:error 'parse-exp "found a malformed let assignment: ~s" exp)
	  ]
	  [(check-improper-list exp always?)
	   (eopl:error 'parse-exp "found an improper list an argument to a let: ~s" exp)
	  ]
	  [(null? (cdr exp)) ; needs an expression to assign
	   (eopl:error 'parse-exp "found a let assignment without a value to assign: ~s" exp)
	  ]
	  [(not (null? (cddr exp))) ; to many things found
	   (eopl:error 'parse-exp "found a let assignment with too many values: ~s" exp)
	  ]
	  [(not (symbol? (assign-sym exp))) ; the first thing isn't a symbol to a assign a value
	   (eopl:error 'parse-exp "found a let assignment with a symbol not as the first parameter: ~s" exp)
	  ]
	  [else ; should be fine now
	   (assign (assign-sym exp) (parse-exp (assign-exp exp)))
	  ]
    )
  )
)

(define parse-lambda
  (lambda (datum)
    (cond
     [(null? (cdr datum)) ; no symbols or body
      (eopl:error 'parse-exp "found an empty lambda: ~s" datum) 
     ]
     [(null? (cddr datum)) ; no body
      (eopl:error 'parse-exp "found lambda with no bodies: ~s" datum)
     ]
     [(symbol? (lambda-vars datum)) ; quick lambda
      (lambda-exp-single (lambda-vars datum) (map parse-exp (lambda-bodies datum)))
     ]
     [((list-of symbol?) (lambda-vars datum)) ; formal lambda
      (lambda-exp (lambda-vars datum) (map parse-exp (lambda-bodies datum)))
     ]
     [else ; assume improper list lambda
      (cond [(not (check-improper-list (lambda-vars datum) symbol?)) ; bad improper list
	     (eopl:error 'parse-exp "found a non-symbol in lambda arguments: ~s" datum)
	    ]
	    [else ; good list
	     (lambda-exp-improper 
	      (improper-list-start (lambda-vars datum))
	      (improper-list-end (lambda-vars datum))
	      (map parse-exp (lambda-bodies datum))
	     )
	    ]
      )
     ]
    )
  )
)

(define parse-and
  (lambda (exp)
    (cond [(null? (cdr exp)) (and-exp (list (lit-exp #t)))]
	  [else (and-exp (map parse-exp (cdr exp)))]
    )
  )
)

(define parse-or
  (lambda (exp)
    (cond [(null? (cdr exp)) (or-exp (list (lit-exp #f)))]
	  [else (or-exp (map parse-exp (cdr exp)))]
    )
  )
)

(define parse-while
  (lambda (exp)
    (cond [(null? (cdr exp))
	   (eopl:error 'parse-exp "No test expression found in a while expression: ~s" exp)
	  ]
	  [(null? (cddr exp))
	   (eopl:error 'parse-exp "No body expressions found in a while expression: ~s" exp)
	  ]
	  [else
	   (while-exp (parse-exp (while-test exp)) (map parse-exp (while-bodies exp))) 
	  ]
    )
  )
)

;(define 
	;unparse-exp
;	(lambda
;		(exp)
;		(cases expression exp
;			[var-exp (id) id]
;			[lit-exp (id) id]
;			[set-bang-exp (id exp)  (list 'set! (unparse-exp id) (unparse-exp exp)  ) ]
;			[if-exp (exp1 exp2)  (list 'if (unparse-exp exp1) (unparse-exp exp2) ) ]
;			[if-else-exp (exp1 exp2 exp3)  (list 'if (unparse-exp exp1) (unparse-exp exp2)  (unparse-exp exp3)  )]
;			[lambda-exp (id body) (append (list 'lambda (map unparse-exp id) ) (map unparse-exp body))]
;			[lambda-exp-single (id body) (append (list 'lambda (unparse-exp id) ) (map unparse-exp body))]
;			[lambda-exp-improper  (required optional body) (list require optional body)]
;			[named-let-exp (name id body) (append (list 'let (unparse-exp name) (map unparse-tuple id) ) (map unparse-exp body))]
;			[let-exp (id body) (append (list 'let (map unparse-tuple id) ) (map unparse-exp body))]
;			[let*-exp (id body) (append (list 'let* (map unparse-tuple id) ) (map unparse-exp body))]
;			[letrec-exp (id body) (append (list 'letrec (map unparse-tuple id) ) (map unparse-exp body))]
;			[app-exp (rator rand)(append (list (unparse-exp rator)  ) (map unparse-exp rand)) ]
;		)
;	)
;)

;(define unparse-tuple (lambda (a) (cases assignment a  [assign (sym value) (list sym (unparse-exp value))])))