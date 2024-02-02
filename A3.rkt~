#lang typed/racket

(require typed/rackunit)

(define-type ExprC (U Real binop IdC LeqC Symbol))

;--------------------------------------------------------------------------
(struct FunDefC ([name : Symbol] [arg : Symbol] [body : ExprC]) #:transparent)
(struct binop ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct IdC ([id : Symbol] [exp : ExprC]) #:transparent)
(struct LeqC ([test : ExprC] [then : ExprC] [rest : ExprC]) #:transparent)

;---------------------------------------------------------------------------
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? r) r]
    [(list (? symbol? s) l r) (binop s (parse l) (parse r))]
    [(list (? symbol? id) exp) (IdC id (parse exp))]
    [(list 'ifleq0? test then rest) (LeqC (parse test) (parse then) (parse rest))]
    [(? symbol? s) s]
    [other (error "Malformed ExprC:" s)]))

(check-equal? (parse '{+ 1 2}) (binop '+ 1 2))
(check-equal? (parse '{+ {* 2 3} 2}) (binop '+ (binop '* 2 3) 2))
(check-equal? (parse '{ifleq0? x {func 7} 8}) (LeqC 'x (IdC 'func 7) 8))
(check-exn (regexp (regexp-quote "Malformed ExprC: '(4 4)"))
           (lambda () (parse '{4 4})))


(define ht (hash '+ + '- - '* * '/ /))

(define (interp [a : ExprC]) : Real
  (match a
    [(? real? r) r]
    [(binop op l r) ((hash-ref ht op) (interp l) (interp r))]
    [(IdC id exp) 0]
    [(LeqC test then rest) (cond
                             [(<= (interp test) 0) (interp then)]
                             [else (interp rest)])]
    [(? symbol? s) 0]))

;-------------------------------------------------------------------------
(define (parse-fundef [s : Sexp]) : FunDefC
  (match s
    [(list 'fun (list (? symbol? id1) (? symbol? id2)) ': expr)
          (FunDefC id1 id2 (parse expr))]
    [other (error "Malformed function structure")]))

(check-equal? (parse-fundef '(fun '(abc bnm) : 1)) (FunDefC 'abc 'bnm 1))




(define (get-fundef [n : Symbol] [fds : (Listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds)
     (error 'get-fundef "reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? n (FunDefC-name (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))






(check-equal? (interp (binop '+ (binop '* 2 3) 2)) 8)
(check-equal? (interp (binop '- (binop '/ 4 2) 1)) 1)
(check-equal? (interp (LeqC -1 2 5)) 2)
(check-equal? (interp (LeqC 3 7 8)) 8)
;place-holder
(check-equal? (interp (IdC 'x 1)) 0)
(check-equal? (interp 'x) 0)
