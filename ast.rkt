#lang typed/racket
(provide (all-defined-out))
(require data-type)

(define-type (GBranch T) (HashTable Symbol T))

#|
Surface AST
|#
(define-type AnonymousValue Value)

(define-type Branch (GBranch Expr))

(data Pat #:prefix
      [Pair Pat Pat]
      [Unit]
      [Var Symbol])

(struct Decl
  ([pattern : Pat]
   [prefix-parameters : (Vectorof Typed)]
   [signature : Expr]
   [body : Expr]
   [recursive? : Boolean]) #:transparent)

(struct Typed
  ([pattern : Pat]
   [expression : Expr])
  #:transparent)

;;; expression
(data Expr #:prefix E
      [Unit]
      [One]
      [Type Level]
      [Void]
      [Var Symbol]
      [Sum Branch]
      [Split Branch]
      [Merge Expr Expr]
      [Pi Typed Expr]
      [Sigma Typed Expr]
      [Lambda Pat (Option AnonymousValue) Expr]
      [First Expr]
      [Second Expr]
      [Application Expr Expr]
      [Pair Expr Expr]
      [Constructor Symbol Expr]
      [Constant Pat Expr Expr]
      [Declaration Decl Expr])

#|
Abstract AST
|#
(define-type Level Natural)

(data (GTelescope V) #:prefix Tele
      [Nil]
      [UpDec (GTelescope V) Decl]
      [UpVar (GTelescope V) Pat V])
(define-type Telescope (GTelescope Value))

;;; closure
(data Clos #:prefix Cl
      [Abstraction Pat (Option Value) Expr Telescope]
      [Value Value]
      [Choice Clos Symbol])

(struct (Expr V) GCase
  ([expr : Expr]
   [context : (GTelescope V)])
  #:transparent)
(define-type Case (GCase (U Value Expr) Value))
(define-type CaseTree (GBranch Case))

(data (GNeutral V) #:prefix GN
      [Generated Symbol]
      [App (GNeutral V) V]
      [First (GNeutral V)]
      [Second (GNeutral V)]
      [Split (GBranch (GCase (U V Expr) V))
             (GNeutral V)])
(define-type Neutral (GNeutral Value))

;;; value
(data Value #:prefix V
      [Lambda Clos]
      [Unit]
      [One]
      [Type Level]
      [Pi Value Clos]
      [Sigma Value Clos]
      [Pair Value Value]
      [Constructor Symbol Value]
      [Split CaseTree]
      [Sum CaseTree]
      [Neu Neutral])

#|
Normal form AST
|#
(define-type NormalCase (GCase (U NormalExpr Expr) NormalExpr))
(define-type NormalCaseTree (GBranch NormalCase))

(define-type NormalNeutral (GNeutral NormalExpr))

(define-type NormalTelescope (GTelescope NormalExpr))

; normal expression
(data NormalExpr #:prefix NE
      [Lambda NormalExpr]
      [Pair NormalExpr NormalExpr]
      [Unit]
      [One]
      [Type Level]
      [Pi NormalExpr NormalExpr]
      [Sigma NormalExpr NormalExpr]
      [Constructor Symbol NormalExpr]
      [Split NormalCaseTree]
      [Sum NormalCaseTree]
      [Neu NormalNeutral])
