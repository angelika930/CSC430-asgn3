#lang typed/racket

(require typed/rackunit)

(define-type ExprC (U Real binop IdC LeqC Symbol))

(struct binop ([op : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct IdC ([id : Symbol] [exp : ExprC]) #:transparent)
(struct LeqC ([test : ExprC] [then : ExprC] [rest : ExprC]) #:transparent)

(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? r) r]
    [(list (? symbol? s) l r) (binop s (parse l) (parse r))]
    [(list (? symbol? id) exp) (IdC id (parse exp))]
    [(list 'ifleq0? test then rest) (LeqC (parse test) (parse then) (parse rest))]
    [(? symbol? s) s]))

(check-equal? (parse '{+ 1 2}) (binop '+ 1 2))
(check-equal? (parse '{+ {* 2 3} 2}) (binop '+ (binop '* 2 3) 2))
(check-equal? (parse '{ifleq0? x {func 7} 8}) (LeqC 'x (IdC 'func 7) 8))