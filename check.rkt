#lang typed/racket
(require "ast.rkt")

(define-type Context (HashTable Symbol Val))

(: lookup : Symbol Context -> Val)
(define (lookup x c) (hash-ref c x))

(: extract-Pi : Val -> (Values Val Clos))
(define (extract-Pi v)
  (match v
    [(Val:Pi t g) (values t g)]
    [else (error 'pi "cannot extract ~a" v)]))

(: extract-Sigma : Val -> (Values Val Clos))
(define (extract-Sigma v)
  (match v
    [(Val:Sigma t g) (values t g)]
    [else (error 'sigma "cannot extract ~a" v)]))

(: Eval : E Telescope -> Val)
(define (Eval e telescope)
  (match e))

(: vfst : Val -> Val)
(define (vfst v)
  (match v
    [(Val:Pair u1 _) u1]
    [(Val:Neu k) (Val:Neu (GNeutral:First k))]
    [else (error 'vfst)]))

(: vsnd : Val -> Val)
(define (vsnd v)
  (match v
    [(Val:Pair _ u2) u2]
    [(Val:Neu k) (Val:Neu (GNeutral:Second k))]
    [else (error 'vsnd)]))

(: Inst : Clos Val -> Val)
(define (Inst clos v)
  (match clos
    [(Clos:Abstraction p opt-v e tele)
     (Eval e (GTelescope:UpVar tele p v))]))

(: update-context : Context Pat Val Val -> Context)
(define (update-context ctx pat v1 v2)
  (match (list pat v1)
    [(list (Pat:Unit) _) ctx]
    [(list (Pat:Var x) t) (hash-set ctx x t)]
    [(list (Pat:Pair p1 p2) (Val:Sigma t g))
     (define new-ctx (update-context ctx p1 t (vfst v2)))
     (update-context new-ctx p2 (Inst g (vfst v2)) (vsnd v2))]
    [else (error 'update-context "p = ~a" pat)]))

(: genV : -> Val)
(define (genV) (Val:Neu (GNeutral:Generated (gensym))))

(: checkT : Telescope Context E -> Void)
(define (checkT tele ctx e)
  (match e
    [(E:Pi (Typed p a) b)
     (checkT tele ctx a)
     (define new-ctx (update-context ctx p (Eval a tele) (genV)))
     (checkT (GTelescope:UpVar tele p (genV)) new-ctx b)]
    [(E:Sigma (Typed p a) b) (checkT tele ctx (E:Pi (Typed p a) b))]
    [(E:Type l) (void)]
    [a (check tele ctx a (Val:Type 0))]))

(: reduce-to-value : Case -> Val)
(define (reduce-to-value case)
  (define e (GCase-expr case))
  (if (E? e)
      (Eval e (GCase-context case))
      e))

(: readbackV : Val -> NE)
(define (readbackV v)
  (match v))

(: eq-normal-form : Val Val -> Void)
(define (eq-normal-form m1 m2)
  (define e1 (readbackV m1))
  (define e2 (readbackV m2))
  (unless (equal? e1 e2)
    (error 'eqnf "~a =/= ~a" e1 e2)))

(: check : Telescope Context E Val -> Void)
(define (check tele ctx e t)
  (match* (e t)
    [((E:Lambda pat _ e) (Val:Pi t g))
     (define gen (genV))
     (define new-ctx (update-context ctx pat t gen))
     (check (GTelescope:UpVar tele pat gen) new-ctx e (Inst g gen))]
    [((E:Pair e1 e2) (Val:Sigma t g))
     (check tele ctx e1 t)
     (check tele ctx e2 (Inst g (Eval e1 tele)))]
    [((E:Constructor constructor-name e) (Val:Sum constructors))
     (define c (hash-ref constructors constructor-name #f))
     (if c
         (check tele ctx e (reduce-to-value c))
         (error 'bad-constructor))]
    [((E:Unit) (Val:One)) (void)]
    [((E:One) (Val:Type 0)) (void)]
    [((E:Pi (Typed pat a) b) (Val:Type 0))
     (check tele ctx a (Val:Type 0))
     (define gen (genV))
     (define new-ctx (update-context ctx pat (Eval a tele) gen))
     (check (GTelescope:UpVar tele pat gen) new-ctx b (Val:Type 0))]
    [((E:Sigma (Typed pat a) b) (Val:Type 0))
     (check tele ctx (E:Pi (Typed pat a) b) (Val:Type 0))]
    [((E:Sum constructors) (Val:Type 0))
     (for ([(_ c) (in-hash constructors)])
       (check tele ctx c (Val:Type 0)))]
    [((E:Declaration d e) t) (define new-ctx (checkD tele ctx d))
                             (check (GTelescope:UpDec tele d) new-ctx e t)]
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
       (check (GTelescope:UpVar tele p gen) new-ctx e t)
       (define v (Eval e (GTelescope:UpDec tele d)))
       (update-context ctx p t v)]
      [else
       (checkT tele ctx a)
       (define t (Eval a tele))
       (check tele ctx e t)
       (update-context ctx p t (Eval e tele))])))

(: checkI : Telescope Context E -> Val)
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
