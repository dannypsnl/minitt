#lang typed/racket
(require "../ast.rkt")

(define-type Context (HashTable String Val))

(: lookup : String Context -> Val)
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

(: genV : Natural -> Val)
(define (genV k) (Val:Neu (GNeutral:Generated k)))

(: checkT : Natural Telescope Context E -> Void)
(define (checkT k tele ctx e)
  (match e
    [(E:Pi (Typed p a) b)
     (checkT k tele ctx a)
     (define new-ctx (update-context ctx p (Eval a tele) (genV k)))
     (checkT (add1 k) (GTelescope:UpVar tele p (genV k)) new-ctx b)]
    [(E:Sigma (Typed p a) b) (checkT k tele ctx (E:Pi (Typed p a) b))]
    [(E:Type l) (void)]
    [a (check k tele ctx a (Val:Type 0))]))

(: check : Natural Telescope Context E Val -> Void)
(define (check k telescope ctx e t)
  (match e
    [else (void)]))

(: checkI : Natural Telescope Context E -> Val)
(define (checkI k telescope ctx e)
  (match e
    [(E:Var x) (lookup x ctx)]
    [(E:Application e1 e2)
     (define t1 (checkI k telescope ctx e1))
     (define-values (t g) (extract-Pi t1))
     (check k telescope ctx e2 t)
     (Inst g (Eval e2 telescope))]
    [(E:First e)
     (define t (checkI k telescope ctx e))
     (define-values (a _) (extract-Sigma t))
     a]
    [(E:Second e)
     (define t (checkI k telescope ctx e))
     (define-values (_ g) (extract-Sigma t))
     (Inst g (vfst (Eval e telescope)))]
    [e (error 'check-infer "cannot infer type of ~a" e)]))
