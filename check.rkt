#lang typed/racket
(provide check)
(require "context.rkt"
         "ast.rkt"
         "readback.rkt"
         "eval.rkt")

(: check : Telescope Context Expr Value -> Void)
(define (check tele ctx e t)
  (match* (e t)
    [((E:Lambda pat _ e) (V:Pi t g))
     (define gen (genV))
     (define new-ctx (update-context ctx pat t gen))
     (check (Tele:UpVar tele pat gen) new-ctx e (Inst g gen))]
    [((E:Pair e1 e2) (V:Sigma t g))
     (check tele ctx e1 t)
     (check tele ctx e2 (Inst g (Eval e1 tele)))]
    [((E:Constructor constructor-name e) (V:Sum constructors))
     (define c (hash-ref constructors constructor-name #f))
     (if c
         (check tele ctx e (reduce-to-value c))
         (error 'bad-constructor))]
    [((E:Split branches) (V:Pi (V:Sum sum-branches) closure))
     (define keys (hash-keys branches))
     (for ([(name branch) (in-hash sum-branches)])
       (set! keys (remove name keys))
       (define pattern-match (hash-ref branches name))
       (define branch-value (reduce-to-value branch))
       (check tele ctx pattern-match (V:Pi branch-value
                                           (Cl:Choice closure name))))
     (unless (empty? keys)
       (error 'check-split))]
    [((E:Unit) (V:One)) (void)]
    [((E:One) (V:Type 0)) (void)]
    [((E:Pi (Typed pat a) b) (V:Type 0))
     (check tele ctx a (V:Type 0))
     (define gen (genV))
     (define new-ctx (update-context ctx pat (Eval a tele) gen))
     (check (Tele:UpVar tele pat gen) new-ctx b (V:Type 0))]
    [((E:Sigma (Typed pat a) b) (V:Type 0))
     (check tele ctx (E:Pi (Typed pat a) b) (V:Type 0))]
    [((E:Sum constructors) (V:Type 0))
     (for ([(_ c) (in-hash constructors)])
       (check tele ctx c (V:Type 0)))]
    [((E:Declaration d e) t) (define new-ctx (checkD tele ctx d))
                             (check (Tele:UpDec tele d) new-ctx e t)]
    [(e t) (define t1 (checkI tele ctx e))
           (eq-normal-form t t1)]))

(: checkD : Telescope Context Decl -> Context)
(define (checkD tele ctx d)
  (let ([a (Decl-signature d)]
        [e (Decl-body d)]
        [p (Decl-pattern d)])
    (cond
      [(Decl-recursive? d)
       (define t (Eval a tele))
       (define gen (genV))
       (define new-ctx (update-context ctx p t gen))
       (check (Tele:UpVar tele p gen) new-ctx e t)
       (define v (Eval e (Tele:UpDec tele d)))
       (update-context ctx p t v)]
      [else
       (checkT tele ctx a)
       (define t (Eval a tele))
       (check tele ctx e t)
       (update-context ctx p t (Eval e tele))])))

(: checkI : Telescope Context Expr -> Value)
(define (checkI telescope ctx e)
  (match e
    [(E:Var x) (lookup x ctx)]
    [(E:Application e1 e2)
     (define t1 (checkI telescope ctx e1))
     (define-values (t g) (extract-Pi t1))
     (check telescope ctx e2 t)
     (Inst g (Eval e2 telescope))]
    [(E:First e)
     (define t (checkI telescope ctx e))
     (define-values (a _) (extract-Sigma t))
     a]
    [(E:Second e)
     (define t (checkI telescope ctx e))
     (define-values (_ g) (extract-Sigma t))
     (Inst g (vfst (Eval e telescope)))]
    [e (error 'check-infer "cannot infer type of ~a" e)]))

(: checkT : Telescope Context Expr -> Void)
(define (checkT tele ctx e)
  (match e
    [(E:Pi (Typed p a) b)
     (checkT tele ctx a)
     (define new-ctx (update-context ctx p (Eval a tele) (genV)))
     (checkT (Tele:UpVar tele p (genV)) new-ctx b)]
    [(E:Sigma (Typed p a) b) (checkT tele ctx (E:Pi (Typed p a) b))]
    [(E:Type l) (void)]
    [a (check tele ctx a (V:Type 0))]))

(: extract-Pi : Value -> (Values Value Clos))
(define (extract-Pi v)
  (match v
    [(V:Pi t g) (values t g)]
    [else (error 'pi "cannot extract ~a" v)]))

(: extract-Sigma : Value -> (Values Value Clos))
(define (extract-Sigma v)
  (match v
    [(V:Sigma t g) (values t g)]
    [else (error 'sigma "cannot extract ~a" v)]))

(: reduce-to-value : Case -> Value)
(define (reduce-to-value case)
  (define e (GCase-expr case))
  (if (Expr? e)
      (Eval e (GCase-context case))
      e))

(: eq-normal-form : Value Value -> Void)
(define (eq-normal-form m1 m2)
  (define e1 (readback-value m1))
  (define e2 (readback-value m2))
  (unless (equal? e1 e2)
    (error 'eqnf "~a =/= ~a" e1 e2)))
