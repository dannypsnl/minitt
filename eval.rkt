#lang typed/racket
(provide (all-defined-out))
(require "ast.rkt")

(: Eval : Expr Telescope -> Value)
(define (Eval e tele)
  (match e
    [(E:Type level) (V:Type level)]
    [(E:Declaration d e) (Eval e (Tele:UpDec tele d))]
    [(E:Lambda pat v e) (V:Lambda (mkCl pat v e tele))]
    [(E:Pi (Typed pat a) b) (V:Pi (Eval a tele) (mkCl pat #f b tele))]
    [(E:Sigma (Typed pat a) b) (V:Sigma (Eval a tele) (mkCl pat #f b tele))]
    [(E:One) (V:One)]
    [(E:Unit) (V:Unit)]
    ; projection for pair
    [(E:First e) (vfst (Eval e tele))]
    [(E:Second e) (vsnd (Eval e tele))]
    [(E:Application e1 e2) (app (Eval e1 tele) (Eval e2 tele))]
    [(E:Var x) (get-telescope tele x)]
    [(E:Pair e1 e2) (V:Pair (Eval e1 tele) (Eval e2 tele))]
    [(E:Constructor c e1) (V:Constructor c (Eval e1 tele))]
    [(E:Sum cas) (V:Sum (for/hash : CaseTree ([(name e) (in-hash cas)])
                          (values name (GCase e tele))))]
    [(E:Split ces) (V:Split (for/hash : CaseTree ([(name e) (in-hash ces)])
                              (values name (GCase e tele))))]
    [else (error 'eval "failed: ~a" e)]))

(: app : Value Value -> Value)
(define (app f v)
  (match* (f v)
    [((V:Lambda f) v) (Inst f v)]
    [((V:Split cases) (V:Constructor c v))
     (define case : Case
       (hash-ref cases c))
     (app (if (Expr? (GCase-expr case))
              (Eval (GCase-expr case) (GCase-context case))
              (GCase-expr case))
          v)]
    [((V:Split s) (V:Neu k)) (V:Neu (GN:Split s k))]
    [((V:Neu k) m) (V:Neu (GN:App k m))]
    [(w u) (error 'app)]))

(: get-telescope : Telescope Symbol -> Value)
(define (get-telescope tele x)
  (match tele
    [(Tele:UpVar tele p v)
     (if (in-pat? x p)
         (pat-proj p x (cast v Value))
         (get-telescope tele x))]
    [(Tele:UpDec tele1 decl)
     (define p (Decl-pattern decl))
     (define e (Decl-body decl))
     (if (in-pat? x p)
         (if (Decl-recursive? decl)
             (pat-proj p x (Eval e tele))
             (pat-proj p x (Eval e tele1)))
         (get-telescope tele1 x))]
    [(Tele:Nil) (error 'get-telescope)]))

(: in-pat? : Symbol Pat -> Boolean)
(define (in-pat? x p)
  (match p
    [(Pat:Var y) (eq? x y)]
    [(Pat:Pair p1 p2) (or (in-pat? x p1) (in-pat? x p2))]
    [(Pat:Unit) #f]))

(: pat-proj : Pat Symbol Value -> Value)
(define (pat-proj pat x v)
  (match pat
    [(Pat:Var y) #:when (eq? x y) v]
    [(Pat:Pair p1 p2) #:when (in-pat? x p1)
                      (pat-proj p1 x (vfst v))]
    [(Pat:Pair p1 p2) #:when (in-pat? x p2)
                      (pat-proj p2 x (vsnd v))]
    [else (error 'pattern-projection)]))

(: mkCl : Pat (Option Value) Expr Telescope -> Clos)
(define (mkCl pat v e tele)
  (Cl:Abstraction pat v e tele))

(: vfst : Value -> Value)
(define (vfst v)
  (match v
    [(V:Pair u1 _) u1]
    [(V:Neu k) (V:Neu (GN:First k))]
    [else (error 'vfst)]))

(: vsnd : Value -> Value)
(define (vsnd v)
  (match v
    [(V:Pair _ u2) u2]
    [(V:Neu k) (V:Neu (GN:Second k))]
    [else (error 'vsnd)]))

(: genV : -> Value)
(define (genV) (V:Neu (GN:Generated (gensym))))

(: Inst : Clos Value -> Value)
(define (Inst clos v)
  (match clos
    [(Cl:Abstraction p opt-v e tele)
     (Eval e (Tele:UpVar tele p v))]))
