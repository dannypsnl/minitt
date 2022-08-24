#lang typed/racket
(provide (all-defined-out))
(require "ast.rkt"
         "eval.rkt")

(: readback-value : Value -> NormalExpr)
(define (readback-value v)
  (match v
    [(V:Lambda f) (NE:Lambda (readback-value (Inst f (genV))))]
    [(V:Pair u v) (NE:Pair (readback-value u) (readback-value v))]
    [(V:Constructor c v) (NE:Constructor c (readback-value v))]
    [(V:Unit) (NE:Unit)]
    [(V:One) (NE:One)]
    [(V:Type level) (NE:Type level)]
    [(V:Pi t g) (NE:Pi (readback-value t) (readback-value (Inst g (genV))))]
    [(V:Sigma t g) (NE:Sigma (readback-value t) (readback-value (Inst g (genV))))]
    [(V:Sum c) (NE:Sum (readback-casetree c))]
    [(V:Neu l) (NE:Neu (readback-neu l))]
    [else (error 'unreachable)]))

(: readback-neu : Neutral -> NormalNeutral)
(define (readback-neu n)
  (match n))

(: readback-casetree : CaseTree -> NormalCaseTree)
(define (readback-casetree tele)
  (for/hash : NormalCaseTree ([(name case) (in-hash tele)])
    (values name
            (readback-case case))))

(: readback-case : Case -> NormalCase)
(define (readback-case case)
  (GCase (if (Value? (GCase-expr case))
             (readback-value (GCase-expr case))
             (GCase-expr case))
         (readback-telescope (GCase-context case))))

(: readback-telescope : (GTelescope Value) -> (GTelescope NormalExpr))
(define (readback-telescope tele)
  (match tele
    [(Tele:Nil) (Tele:Nil)]
    [(Tele:UpDec ctx decl)
     (Tele:UpDec (readback-telescope ctx) decl)]
    [(Tele:UpVar ctx pat val)
     (Tele:UpVar (readback-telescope ctx)
                 pat
                 (readback-value (cast val Value)))]
    [else (error 'unreachable)]))
