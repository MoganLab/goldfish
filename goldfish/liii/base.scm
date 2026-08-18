(define-library (liii base)
  (import (scheme base) (srfi srfi-2) (srfi srfi-8) (goldfish))
  (export and-let*
    receive
    define*
    lambda*
    object->string
    eval-string
    signature
    copy
    keyword?
    string->keyword
    symbol->keyword
    keyword->symbol
    loose-car
    loose-cdr
    compose
    typed-lambda
    make-hook
    hook-functions
    with-output-to-string
    with-input-from-string
    call-with-input-string
    call-with-output-string
    reverse!
    format
  ) ;export
  (begin

    (define (loose-car pair-or-empty)
      (if (eq? '() pair-or-empty) '() (car pair-or-empty))
    ) ;define

    (define (loose-cdr pair-or-empty)
      (if (eq? '() pair-or-empty) '() (cdr pair-or-empty))
    ) ;define

    (define (compose . fs)
      (if (null? fs)
        (lambda (x) x)
        (lambda (x) ((car fs) ((apply compose (cdr fs)) x)))
      ) ;if
    ) ;define

    (define-macro (typed-lambda args . body)
      (if (symbol? args)
        (apply lambda args body)
        (let ((new-args (copy args)))
          (do ((p new-args (cdr p)))
            ((not (pair? p)))
            (if (pair? (car p)) (set-car! p (caar p)))
          ) ;do
          `(lambda ,new-args
             ,@(map (lambda (arg)
                      (if (pair? arg)
                        `(unless (,(cadr arg) ,(car arg))
                           (error 'type-error
                             ,"~S is not ~S~%"
                             (quote ,(car arg))
                             (quote ,(cadr arg))))
                        (values)))
                 args)
             ,@body)
        ) ;let
      ) ;if
    ) ;define-macro

  ) ;begin
) ;define-library
