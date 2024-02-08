#lang typed/racket

(require typed/rackunit)

;--------------------------------------------------------------------------
(struct FunDefC ([name : Symbol] [params : (Listof Symbol)] [body : ExprC]) #:transparent)

(define-type ExprC (U Real binop AppC LeqC Symbol))
(struct binop ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct AppC ([id : Symbol] [args : (Listof ExprC)]) #:transparent)
(struct LeqC ([test : ExprC] [then : ExprC] [rest : ExprC]) #:transparent)

(define ht (hash '+ + '- - '* * '/ /))

;---------------------------------------------------------------------------

;Checks if a given argument is a binop
(: is-binop? (Any -> Boolean : #:+ Symbol))
(define (is-binop? sym)
  (and (symbol? sym) (hash-has-key? ht sym)))

;Checks if a given argument is a keyword
(: not-keyword? (Any -> Boolean : #:+ Symbol))
(define (not-keyword? sym)
  (and (symbol? sym) (not (hash-has-key? ht sym)) (not (or (eq? 'ifleq0? sym) (eq? 'func sym) (eq? ': sym)))))

;---------------------------------------------------------------------------

;Parses an Sexpression into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? r) r]
    [(list (? is-binop? op) l r) (binop op (parse l) (parse r))]
    [(list (? not-keyword? id) exps ...) (AppC id (map parse exps))]
    [(list 'ifleq0? test then rest) (LeqC (parse test) (parse then) (parse rest))]
    [(? not-keyword? s) s]
    [other (error "OAZO Malformed ExprC:" s)]))


;Parses a Sexp into a FunDefC
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'func (list (? not-keyword? id1) (? not-keyword? ids) ...) ': expr)
     (FunDefC id1 (cast ids (Listof Symbol)) (parse expr))]
    [other (error "OAZO Malformed function structure")])) 

;Parse the entire program 
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]
    [other (error "OAZO Malformed Program:" s)]))

;---------------------------------------------------------------------------

;Takes in a list of ExprC and substitutes variables one at a time
(define (subst-multiarg [what : (Listof ExprC)] [for : (Listof Symbol)] [in : ExprC]) : ExprC
  (match what
    ['() in]
    [(cons f r) (subst f (first for) (subst-multiarg r (rest for) in))]))




;Replaces all occurences of 'what' with 'for' from 'in'
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(? real? n) in]
    [(? symbol? s) (cond
                     [(symbol=? s for) what]
                     [else in])]
    [(AppC f a) (AppC f (map (lambda ([expr : ExprC])(subst what for expr)) a))]
    [(binop op l r) (binop op (subst what for l)
                           (subst what for r))]
    [(LeqC test then rest) (LeqC (subst what for test)
                                 (subst what for then)
                                 (subst what for rest))]))

;Gets a FunDefC from a list with name n
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds)
     (error 'get-fundef "OAZO reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? n (FunDefC-name (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))

;-------------------------------------------------------------------------


;Interprets an AST (as an ExprC) into a real number
(define (interp [a : ExprC] [fds : (Listof FunDefC)]) : Real
  (match a
    [(? real? r) r]
    [(binop op l r) (cond
                       [(and (eq? op '/) (eq? (interp r fds) 0)) (error "OAZO Divide by 0 error")]
                       [else ((hash-ref ht op) (interp l fds) (interp r fds))])]
    [(AppC id Listexp)
     (match (get-fundef id fds)
       [(FunDefC name params body) (interp (subst-multiarg  (map (lambda ([expr : ExprC]) (interp expr fds)) Listexp) params body) fds)])]
    [(LeqC test then rest) (cond
                             [(<= (interp test fds) 0) (interp then fds)]
                             [else (interp rest fds)])]
    [other (error "OAZO Malformed ExprC:" a)]))


;Find the main function and interp it
(define (interp-fns [l : (Listof FunDefC)]) : Real
  (interp (AppC 'main '()) l))


;Wrapper for our praser and interpretor 
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

;-------------------------------------------------------------------------

;is-binop?
(check-equal? (is-binop? '*) #t)
(check-equal? (is-binop? 'f) #f)

;not-keyword?
(check-equal? (not-keyword? '*) #f)
(check-equal? (not-keyword? '+) #f)
(check-equal? (not-keyword? 'abc) #t)

;parse
(check-equal? (parse '{+ 1 2}) (binop '+ 1 2))
(check-equal? (parse '{+ {* 2 3} 2}) (binop '+ (binop '* 2 3) 2))
(check-equal? (parse '{ifleq0? x {fun 7} 8}) (LeqC 'x (AppC 'fun '(7)) 8))
(check-exn (regexp (regexp-quote "OAZO Malformed ExprC: '(4 4)"))
           (lambda () (parse '{4 4})))
(check-exn (regexp (regexp-quote "OAZO Malformed ExprC: '/"))
           (lambda () (parse '{+ / 3})))

;parse-fundef
(check-equal? (parse-fundef '(func (abc bnm) : 1)) (FunDefC 'abc '(bnm) 1)) 
(check-exn (regexp (regexp-quote "OAZO Malformed function structure"))
           (lambda () (parse-fundef '{4})))
(check-exn (regexp (regexp-quote "OAZO Malformed function structure"))
           (lambda () (parse-fundef '{func {+ 4} : 13})))

;parse-prog
(check-equal? (parse-prog '{{func {fun y} : {+ 1 2}} {func {fun2 x} : 1}})
              (list (FunDefC 'fun '(y) (binop '+ 1 2)) (FunDefC 'fun2 '(x) 1)))
(check-exn (regexp (regexp-quote "OAZO Malformed Program: 'x"))
           (lambda () (parse-prog 'x)))

;subst
(check-equal? (subst 3 'x (LeqC 1 'x (binop '+ 'x 'y))) (LeqC 1 3 (binop '+ 3 'y)))
(check-equal? (subst 4 'y (AppC 'fun '(y))) (AppC 'fun '(4)))

;get-fundef
(check-exn (regexp (regexp-quote "OAZO reference to undefined function"))
           (lambda () (get-fundef 'x '())))
(check-equal? (get-fundef 'subtract (list (FunDefC 'add '(+) 4) (FunDefC 'mult '(*) 2)
                                          (FunDefC 'subtract '(-) 4))) (FunDefC 'subtract '(-) 4))
(check-exn (regexp (regexp-quote "OAZO reference to undefined function" ))
           (lambda () (get-fundef 'x (list (FunDefC 'add '(+) 4)))))


;interp
(check-equal? (interp (binop '+ (binop '* 2 3) 2) '()) 8)
(check-equal? (interp (binop '- (binop '/ 4 2) 1) '()) 1)
(check-equal? (interp (LeqC -1 2 5) '()) 2)
(check-equal? (interp (LeqC 3 7 8) '()) 8)
(check-equal? (interp (AppC 'x '(1)) (list (FunDefC 'x '(y) (binop '+ 'y 1)))) 2)
(check-exn (regexp (regexp-quote "OAZO Malformed ExprC: 'x"))
           (lambda () (interp 'x '())))
(check-exn (regexp (regexp-quote "OAZO Divide by 0 error"))
           (lambda () (interp (binop '/ 5 0) '())))

;interp-fns
(check-equal? (interp-fns (list (FunDefC 'fun2 '(x) 1) (FunDefC 'main '() (binop '+ 1 2)))) 3)

;top-interp
(define prog '{
               {func {fib x} : {ifleq0? {- x 1} 1 {+ {fib {- x 1}} {fib {- x 2}}}}}
               {func {main init} : {fib 4}}
               })

(define prog2 ' {
                 {func {sqr x} : {* x x}}
                 {func {main init} : {sqr 7}}
                 })
;round
(define round '{func {round x} : {ifleq0? {- x 0.499999} 0 {+ 1 {round {- x 1}}}}})
(define round-top '{func {round-top x} : {ifleq0? x {- 0 {round {- 0 x}}} {round x}}})
(check-equal? (top-interp (list round round-top '{func {main init} : {round-top 7.5}})) 8)
(check-equal? (top-interp (list round round-top '{func {main init} : {round-top -7.4}})) -7)

(check-equal? (top-interp prog) 5)
(check-equal? (top-interp prog2) 49)
(check-equal? (interp-fns
               (parse-prog '{{func {f x} : {+ x 14}}
                             {func {main init} : {f 2}}}))
              16)

(define prog3 '{
                {func {f x x} : x}
                {func {main} : {f x}}
                })

(define prog4 '{
                {func {five} : 5}
                {func {main} : {five}}
                })

(define prog5 '{
                {func {f x} : (+ x 2)}
                {func {main} : {f 3 4 5}}
                })
;(check-equal? (top-interp prog5) '(5 6 7))
;expected exception with message containing OAZO on test expression: '(top-interp '((func (f x) : (+ x 2)) (func (main) : (f 3 4 5))))

;round
(define round '{func {round x} : {ifleq0? {- x 0.499999} 0 {+ 1 {round {- x 1}}}}})
(define round-top '{func {round-top x} : {ifleq0? x {- 0 {round {- 0 x}}} {round x}}})
(check-equal? (top-interp (list round round-top '{func {main init} : {round-top 7.5}})) 8)
(check-equal? (top-interp (list round round-top '{func {main init} : {round-top -7.4}})) -7)

(check-equal? (top-interp prog) 5)
(check-equal? (top-interp prog2) 49)
(check-equal? (interp-fns
               (parse-prog '{{func {f x} : {+ x 14}}
                             {func {main init} : {f 2}}}))
              16)

;testing for multiple parameters

(check-exn (regexp (regexp-quote "OAZO Malformed ExprC: 'x"))
           (lambda () (top-interp prog3)))

(check-equal? (interp-fns
       (parse-prog '{{func {f x y} : {+ x y}}
                     {func {main} : {f 1 2}}}))
      3)

 (check-exn #px"OAZO Malformed ExprC: 'y"
            (λ ()
              (interp-fns
               (parse-prog '{{func {f x y} : {+ x y}}
                             {func {main} : {f 1}}}))))

;Testing for zero parameters
(check-equal? (top-interp prog4) 5)



