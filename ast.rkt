#lang typed/racket
(provide (all-defined-out))
(require data-type)

(define-type (GenericBranch T) (HashTable String T))

#|
Surface AST
|#
(define-type AnonymousValue Val)

(define-type Branch (GenericBranch E))

(data Pat
      [Pair Pat Pat]
      [Unit]
      [Var String])

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
      [Var String]
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
      [Constructor String E]
      [Constant Pat E E]
      [Declaration Decl E])

#|
Abstract AST
|#
(define-type Level Positive-Integer)

(data (GenericTelescope V)
      [Nil]
      [UpDec (GenericTelescope V) Decl]
      [UpVar (GenericTelescope V) Pat Val])
(define-type Telescope (GenericTelescope Val))

;;; closure
(data Clos
      [Abstraction Pat (Option Val) E Telescope]
      [Value Val]
      [Choice Clos String])

(struct (E V) GenericCase
  ([expr : E]
   [context : (GenericTelescope V)])
  #:transparent)
(define-type Case (GenericCase (U Val E) Val))
(define-type CaseTree (GenericBranch Case))

(data (GenericNeutral V)
      [Generated Natural]
      [Application (GenericNeutral V) V]
      [First (GenericNeutral V)]
      [Second (GenericNeutral V)]
      [Split (GenericBranch (GenericCase (U V E) V))
             (GenericNeutral V)])
(define-type Neutral (GenericNeutral Val))

;;; value
(data Val
      [Lambda Clos]
      [Unit]
      [One]
      [Type Level]
      [Pi Val Clos]
      [Sigma Val Clos]
      [Pair Val Val]
      [Constructor String Val]
      [Split CaseTree]
      [Sum CaseTree]
      [Neutral Neutral])

#|
Normal form AST
|#
(define-type NormalCase (GenericCase (U NormE E) NormE))
(define-type NormalCaseTree (GenericBranch NormalCase))

(define-type NormalNeutral (GenericNeutral NormE))

; normal expression
(data NormE
      [Lambda Natural NormE]
      [Pair NormE NormE]
      [Unit]
      [One]
      [Type Level]
      [Pi NormE Natural NormE]
      [Sigma NormE Natural NormE]
      [Constructor String NormE]
      [Split NormalCaseTree]
      [Sum NormalCaseTree]
      [Neutral NormalNeutral])
