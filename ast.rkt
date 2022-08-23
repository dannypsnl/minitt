#lang typed/racket
(provide (all-defined-out))
(require data-type)

(define-type (GBranch T) (HashTable Symbol T))

#|
Surface AST
|#
(define-type AnonymousValue Val)

(define-type Branch (GBranch E))

(data Pat
      [Pair Pat Pat]
      [Unit]
      [Var Symbol])

(struct Decl
  ([pattern : Pat]
   [prefix-parameters : (Vectorof Typed)]
   [signature : E]
   [body : E]
   [recursive? : Boolean]) #:transparent)

(struct Typed
  ([pattern : Pat]
   [expression : E])
  #:transparent)

;;; expression
(data E
      [Unit]
      [One]
      [Type Level]
      [Void]
      [Var Symbol]
      [Sum Branch]
      [Split Branch]
      [Merge E E]
      [Pi Typed E]
      [Sigma Typed E]
      [Lambda Pat (Option AnonymousValue) E]
      [First E]
      [Second E]
      [Application E E]
      [Pair E E]
      [Constructor Symbol E]
      [Constant Pat E E]
      [Declaration Decl E])

#|
Abstract AST
|#
(define-type Level Natural)

(data (GTelescope V)
      [Nil]
      [UpDec (GTelescope V) Decl]
      [UpVar (GTelescope V) Pat Val])
(define-type Telescope (GTelescope Val))

;;; closure
(data Clos
      [Abstraction Pat (Option Val) E Telescope]
      [Value Val]
      [Choice Clos Symbol])

(struct (E V) GCase
  ([expr : E]
   [context : (GTelescope V)])
  #:transparent)
(define-type Case (GCase (U Val E) Val))
(define-type CaseTree (GBranch Case))

(data (GNeutral V)
      [Generated Symbol]
      [App (GNeutral V) V]
      [First (GNeutral V)]
      [Second (GNeutral V)]
      [Split (GBranch (GCase (U V E) V))
             (GNeutral V)])
(define-type Neutral (GNeutral Val))

;;; value
(data Val
      [Lambda Clos]
      [Unit]
      [One]
      [Type Level]
      [Pi Val Clos]
      [Sigma Val Clos]
      [Pair Val Val]
      [Constructor Symbol Val]
      [Split CaseTree]
      [Sum CaseTree]
      [Neu Neutral])

#|
Normal form AST
|#
(define-type NormalCase (GCase (U NE E) NE))
(define-type NormalCaseTree (GBranch NormalCase))

(define-type NormalNeutral (GNeutral NE))

; normal expression
(data NE
      [Lambda Natural NE]
      [Pair NE NE]
      [Unit]
      [One]
      [Type Level]
      [Pi NE Natural NE]
      [Sigma NE Natural NE]
      [Constructor Symbol NE]
      [Split NormalCaseTree]
      [Sum NormalCaseTree]
      [Neutral NormalNeutral])
