#lang typed/racket
(require "ast.rkt"
         "check.rkt")

(: check-main : Expr -> Void)
(define (check-main e)
  (check (Tele:Nil) (hash) e (V:One)))

(module+ test
  ; const (p : A) := M
  (check-main (E:Constant (Pat:Var 'bool)
                          (E:Type 1)
                          (E:Type 0))))
