(library (srfi :262 match)
  (export match
          match-lambda
          match-values
          match-let
          match-let*
          match-let-values
          match-let*-values
          match-define
          match-define-values
          match-letrec
          match-letrec*
          if-match

          define-pattern-syntax
          match-ellipsis?

          &match
          make-match-violation
          match-violation?

          and or not => ? quote
          seq seq* seq/unordered
          cons cons* lset list vector eof-object
          quasiquote unquote unquote-splicing)
  (import (extensible-match)))
