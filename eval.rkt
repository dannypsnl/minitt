#lang typed/racket
(provide (all-defined-out))
(require "ast.rkt")

(: genV : -> Value)
(define (genV) (V:Neu (GN:Generated (gensym))))

(: Eval : Expr Telescope -> Value)
(define (Eval e telescope)
  (match e))

(: Inst : Clos Value -> Value)
(define (Inst clos v)
  (match clos
    [(Cl:Abstraction p opt-v e tele)
     (Eval e (Tele:UpVar tele p v))]))
