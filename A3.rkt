#lang typed/racket

(require typed/rackunit)

;--------------------------------------------------------------------------
(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)

(define-type ExprC (U Real binop AppC LeqC Symbol))
(struct binop ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct AppC ([id : Symbol] [exp : ExprC]) #:transparent)
(struct LeqC ([test : ExprC] [then : ExprC] [rest : ExprC]) #:transparent)

(define ht (hash '+ + '- - '* * '/ /))

;---------------------------------------------------------------------------

;Parses an Sexpression into an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? r) r]
    [(list (? symbol? s) l r) (binop s (parse l) (parse r))]
    [(list (? symbol? id) exp) (AppC id (parse exp))]
    [(list 'ifleq0? test then rest) (LeqC (parse test) (parse then) (parse rest))]
    [(? symbol? s) s]
    [other (error "OAZO Malformed ExprC:" s)]))

(check-equal? (parse '{+ 1 2}) (binop '+ 1 2))
(check-equal? (parse '{+ {* 2 3} 2}) (binop '+ (binop '* 2 3) 2))
(check-equal? (parse '{ifleq0? x {func 7} 8}) (LeqC 'x (AppC 'func 7) 8))
(check-exn (regexp (regexp-quote "OAZO Malformed ExprC: '(4 4)"))
           (lambda () (parse '{4 4})))

;---------------------------------------------------------------------------


;Parses a Sexp into a FunDefC
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'func (list (? symbol? id1) (? symbol? id2)) ': expr)
          (FunDefC id1 id2 (parse expr))]
    [other (error "OAZO Malformed function structure")])) 

(check-equal? (parse-fundef '(func (abc bnm) : 1)) (FunDefC 'abc 'bnm 1))
(check-exn (regexp (regexp-quote "OAZO Malformed function structure"))
           (lambda () (parse-fundef '{4})))


;Replaces all occurences of 'what' with 'for' from 'in'
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(? real? n) in]
    [(? symbol? s) (cond
                     [(symbol=? s for) what]
                     [else in])]
    [(AppC f a) (AppC f (subst what for a))]
    [(binop op l r) (binop op (subst what for l)
                           (subst what for r))]
    [(LeqC test then rest) (LeqC (subst what for test)
                                 (subst what for then)
                                 (subst what for rest))]))

(check-equal? (subst 3 'x (LeqC 1 'x (binop '+ 'x 'y))) (LeqC 1 3 (binop '+ 3 'y))) 
(check-equal? (subst 4 'y (AppC 'fun 'y)) (AppC 'fun 4))

;Gets a FunDefC from a list with name n 
(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds)
     (error 'get-fundef "OAZO reference to undefined function")] 
    [(cons? fds)
     (cond
       [(equal? n (FunDefC-name (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))

(check-exn (regexp (regexp-quote "OAZO reference to undefined function"))
           (lambda () (get-fundef 'x '())))
(check-equal? (get-fundef 'subtract (list (FunDefC 'add '+ 4) (FunDefC 'mult '* 2) (FunDefC 'subtract '- 4))) (FunDefC 'subtract '- 4))
(check-exn (regexp (regexp-quote "OAZO reference to undefined function" ))
           (lambda () (get-fundef 'x (list (FunDefC 'add '+ 4)))))

;Interprets an AST (as an ExprC) into a real number 
(define (interp [a : ExprC] [fds : (Listof FunDefC)]) : Real
  (match a
    [(? real? r) r]
    [(binop op l r) ((hash-ref ht op) (interp l fds) (interp r fds))]
    [(AppC id exp)
     (match (get-fundef id fds)
       [(FunDefC name param body) (interp (subst (interp exp fds) param body) fds)])]
    [(LeqC test then rest) (cond
                             [(<= (interp test fds) 0) (interp then fds)]
                             [else (interp rest fds)])]
    [other (error "OAZO Malformed ExprC:" a)]))


(check-equal? (interp (binop '+ (binop '* 2 3) 2) '()) 8)
(check-equal? (interp (binop '- (binop '/ 4 2) 1) '()) 1)
(check-equal? (interp (LeqC -1 2 5) '()) 2)
(check-equal? (interp (LeqC 3 7 8) '()) 8)

(check-equal? (interp (AppC 'x 1) (list (FunDefC 'x 'y (binop '+ 'y 1)))) 2)
(check-exn (regexp (regexp-quote "OAZO Malformed ExprC: 'x"))
           (lambda () (interp 'x '())))

;-------------------------------------------------------------------------
(define (parse-prog [s : Sexp]) : (Listof FunDefC)
  (match s
    ['() '()]
    [(cons f r) (cons (parse-fundef f) (parse-prog r))]
    [other (error "OAZO Malformed Program:" s)]))

(check-equal? (parse-prog '{{func {fun y} : {+ 1 2}} {func {fun2 x} : 1}})
              (list (FunDefC 'fun 'y (binop '+ 1 2)) (FunDefC 'fun2 'x 1)))
(check-exn (regexp (regexp-quote "OAZO Malformed Program: 'x"))
           (lambda () (parse-prog 'x)))

(define (interp-fns [l : (Listof FunDefC)]) : Real
  ;(match (get-fundef 'main l)
   ; [(FunDefC 'main arg body) (interp body l)]))
  (interp (AppC 'main 0) l))

(check-equal? (interp-fns (list (FunDefC 'fun2 'x 1) (FunDefC 'main 'init (binop '+ 1 2)))) 3)

(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

(define prog '{
               {func {fib x} : {ifleq0? {- x 1} 1 {+ {fib {- x 1}} {fib {- x 2}}}}}
               {func {main init} : {fib 4}}
               })

(define prog2 ' {
                 {func {sqr x} : {* x x}}
                 {func {main init} : {sqr 7}}
                 }) 

(check-equal? (top-interp prog) 5)
(check-equal? (top-interp prog2) 49)
(check-equal? (interp-fns
               (parse-prog '{{func {f x} : {+ x 14}}
                             {func {main init} : {f 2}}}))
              16)


