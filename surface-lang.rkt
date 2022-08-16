#lang typed/racket
(require data-type)

(data Value)

(define-type AnonymousValue Value)

(define-type Level Positive-Integer)
(define-type Branch (HashTable String Expression))

(struct Decl
  ([pattern : Pattern]
   [prefix-parameters : (Vectorof Typed)]
   [signature : Expression]
   [body : Expression]
   [recursive? : Boolean]) #:transparent)

(struct Typed
  ([pattern : Pattern]
   [expression : Expression])
  #:transparent)

(data Pattern
      [PatPair Pattern Pattern]
      [PatUnit]
      [PatVar String])

(data Expression
      [Unit]
      [One]
      [Type Level]
      [Void]
      [Var String]
      [Sum Branch]
      [Split Branch]
      [Merge Expression Expression]
      [Pi Typed Expression]
      [Sigma Typed Expression]
      [Lambda Pattern (Option AnonymousValue) Expression]
      [First Expression]
      [Second Expression]
      [Application Expression Expression]
      [Pair Expression Expression]
      [Constructor String Expression]
      [Constant Pattern Expression Expression]
      [Declaration Decl Expression])
