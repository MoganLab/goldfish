(define-library (liii base)
  (import (scheme base) (srfi srfi-2) (srfi srfi-8))
  (export and-let* receive define* lambda* define-macro define-bacro
    define*-record-type procedure-source procedure-arglist arity defined?
    object->string eval-string signature copy keyword? string->keyword
    symbol->keyword keyword->symbol loose-car loose-cdr compose identity any?
    make-hook hook-functions with-output-to-string with-input-from-string
    call-with-input-string call-with-output-string reverse! sort! format
    cyclic-sequences
  ) ;export
  (begin

    (define (loose-car pair-or-empty)
      (if (eq? '() pair-or-empty) '() (car pair-or-empty))
    ) ;define

    (define (loose-cdr pair-or-empty)
      (if (eq? '() pair-or-empty) '() (cdr pair-or-empty))
    ) ;define

    (define identity (lambda (x) x))

    (define (compose . fs)
      (if (null? fs)
        (lambda (x) x)
        (lambda (x) ((car fs) ((apply compose (cdr fs)) x)))
      ) ;if
    ) ;define

    (define (any? x)
      #t
    ) ;define

    (define-syntax %define*-record-type-aux
      (syntax-rules ()
        ((_ <type-name> ctor orig-params (field ...) () pred clause ...)
         (begin
           (define-record-type <type-name>
             (%raw-ctor field ...)
             pred
             clause
             ...
           ) ;define-record-type
           (define* (ctor . orig-params) (%raw-ctor field ...))
         ) ;begin
        ) ;
        ((_ <type-name>
           ctor
           orig-params
           (field ...)
           ((f default) . rest)
           pred
           clause
           ...
         ) ;_
         (%define*-record-type-aux <type-name>
           ctor
           orig-params
           (field ... f)
           rest
           pred
           clause
           ...
         ) ;%define*-record-type-aux
        ) ;
        ((_ <type-name> ctor orig-params (field ...) (f . rest) pred clause ...)
         (%define*-record-type-aux <type-name>
           ctor
           orig-params
           (field ... f)
           rest
           pred
           clause
           ...
         ) ;%define*-record-type-aux
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

    (define-syntax define*-record-type
      (syntax-rules ()
        ((_ <type-name> (ctor . params) pred clause ...)
         (%define*-record-type-aux <type-name> ctor params () params pred clause ...)
        ) ;
      ) ;syntax-rules
    ) ;define-syntax

  ) ;begin
) ;define-library
