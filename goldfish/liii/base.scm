;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii base)
  (import (scheme base)
          (srfi srfi-2)
          (srfi srfi-8)
  ) ;import
  (re-export
    ; (scheme base) defined by R7RS
    let-values
    ; R7RS 5: Program Structure
    define-values define-record-type
    ; R7RS 6.2: Numbers
    square exact inexact max min floor floor/ s7-floor ceiling s7-ceiling truncate truncate/ s7-truncate
    round s7-round floor-quotient floor-remainder gcd lcm s7-lcm modulo exact-integer-sqrt
    numerator denominator exact-integer? number->string string->number
    ; R7RS 6.3: Booleans
    boolean=?
    ; R7RS 6.4: list
    pair? cons car cdr set-car! set-cdr! caar cadr cdar cddr
    null? list? make-list list length append reverse list-tail
    list-ref list-set! memq memv member assq assv assoc list-copy
    ; R7RS 6.5: Symbol
    symbol? symbol=? string->symbol symbol->string
    ; R7RS 6.6: Characters
    digit-value
    ; R7RS 6.7: String
    string-copy
    ; R7RS 6.8 Vector
    vector->string string->vector vector-copy vector-copy! vector-fill!
    ; R7RS 6.9 Bytevectors
    bytevector? make-bytevector bytevector bytevector-length bytevector-u8-ref
    bytevector-u8-set! bytevector-copy bytevector-append
    utf8->string string->utf8 utf8-string-length u8-substring bytevector-advance-utf8
    ; Input and Output
    call-with-port port? binary-port? textual-port? input-port-open? output-port-open?
    open-binary-input-file open-binary-output-file close-port eof-object
    ; Control flow
    string-map vector-map string-for-each vector-for-each
    ; Exception
    raise guard read-error? file-error?
    ; SRFI-2
    and-let*
    ; SRFI-8
    receive
    ; S7 extensions
    define*
    procedure-source
    procedure-arglist
    arity
    defined?
    object->string
    eval-string
    signature
    ; Keywords
    keyword?
    string->keyword
    symbol->keyword
    keyword->symbol
  ) ;re-export
  (export
    ; workaround for binding s7 primitives
    (rename vector-append vector-append)
    ; Extra routines
    loose-car
    loose-cdr
    compose
    identity
    any?
    ; Extra structure
    typed-lambda
  ) ;export
  (begin

    (define (loose-car pair-or-empty)
      (if (eq? '() pair-or-empty)
          '()
          (car pair-or-empty)
      ) ;if
    ) ;define

    (define (loose-cdr pair-or-empty)
      (if (eq? '() pair-or-empty)
          '()
          (cdr pair-or-empty)
      ) ;if
    ) ;define

    (define identity (lambda (x) x))

    (define (compose . fs)
      (if (null? fs)
          (lambda (x) x)
          (lambda (x)
            ((car fs) ((apply compose (cdr fs)) x))
          ) ;lambda
      ) ;if
    ) ;define

    (define (any? x) #t)

    (define-syntax let1
      (syntax-rules ()
        ((_ name1 value1 body ...)
         (let ((name1 value1))
           body ...))))

    (define-syntax typed-lambda
      (lambda (stx)
        (define (split-args args)
          (let loop ((args args))
            (syntax-case args ()
              ;; 结束条件
              (() (values '() '()))

              ;; 带有类型的变量: ((var type) . rest)
              (((var type) . rest)
               (let-values (((clean-rest checks-rest) (loop (syntax rest))))
                 (values (cons (syntax var) clean-rest)
                         (cons #'(unless (type var)
                                   (error 'type-error "~S is not ~S" 'var 'type))
                               checks-rest))))

              ;; 普通变量或 rest 变量: (var . rest) 或只是 var
              ((var . rest)
               (let-values (((clean-rest checks-rest) (loop (syntax rest))))
                 (values (cons (syntax var) clean-rest) checks-rest)))

              ;; 点号后面的最后一个标识符 (例如 rest)
              (var
               (if (identifier? (syntax var))
                   (values (syntax var) '())
                   (raise-syntax-error #f "Invalid argument specification" stx (syntax var)))))))

        (syntax-case stx ()
          ((_ args body1 body2 ...)
           (let-values (((clean-args checks) (split-args (syntax args))))
             (with-syntax ((clean-args clean-args)
                           ((check ...) checks)
                           ((body ...) #'(body1 body2 ...)))
               #'(lambda clean-args
                   check ...
                   body ...)))))))

  ) ; end of begin
) ; end of define-library
