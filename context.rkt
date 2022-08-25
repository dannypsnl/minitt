#lang typed/racket
(provide Context
         lookup
         update-context)
(require "ast.rkt"
         "eval.rkt")

(define-type Context (HashTable Symbol Value))
(: lookup : Symbol Context -> Value)
(define (lookup x c) (hash-ref c x))

(: update-context : Context Pat Value Value -> Context)
(define (update-context ctx pat v1 v2)
  (match (list pat v1)
    [(list (Pat:Unit) _) ctx]
    [(list (Pat:Var x) t) (hash-set ctx x t)]
    [(list (Pat:Pair p1 p2) (V:Sigma t g))
     (define new-ctx (update-context ctx p1 t (vfst v2)))
     (update-context new-ctx p2 (Inst g (vfst v2)) (vsnd v2))]
    [else (error 'update-context "p = ~a" pat)]))
