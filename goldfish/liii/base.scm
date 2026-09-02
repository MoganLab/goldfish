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

    (define-syntax typed-lambda
      (lambda (stx)
        (syntax-case stx ()
          ((typed-lambda args body ...)
           (let ((args-datum (syntax->datum #'args))
                 (body-datum (syntax->datum #'(body ...))))
             (if (symbol? args-datum)
               (datum->syntax stx `(lambda ,args-datum ,@body-datum))
               (let ((new-args (let ((c (copy args-datum)))
                                 (let loop ((p c))
                                   (when (pair? p)
                                     (when (pair? (car p)) (set-car! p (caar p)))
                                     (loop (cdr p))))
                                 c)))
                 (datum->syntax stx
                   `(lambda ,new-args
                      ,@(map (lambda (arg)
                               (if (pair? arg)
                                 `(unless (,(cadr arg) ,(car arg))
                                    (error 'type-error "~S is not ~S~%" ',(car arg) ',(cadr arg)))
                                 '(values)))
                          args-datum)
                      ,@body-datum)))))))))

  ) ;begin
) ;define-library
