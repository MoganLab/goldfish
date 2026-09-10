(define-library (liii check)
  (import (goldfish))
  (export test
    check
    check-approx
    check-set-mode!
    check:proc
    check-catch
    check-report
    check-failed?
    check-true
    check-false
  ) ;export
  (import (scheme base)
    (srfi srfi-78)
    (rename (srfi srfi-78) (check-report srfi-78-check-report))
  ) ;import
  (begin

    (define-syntax check-true
      (syntax-rules ()
        ((check-true body) (check body => #t))))

    (define-syntax check-false
      (syntax-rules ()
        ((check-false body) (check body => #f))))

    ;; (check-approx expr => expected opts ...) is the documented form
    ;; (matching srfi-78's check); the arrowless form stays accepted.
    ;; Self-contained transformer (v5): the option parser and builder are
    ;; letrec*-bound inside the closure -- own value definitions are
    ;; phase-0 only, so expansion-time helpers cannot be sibling defines.
    (define-syntax check-approx
      (lambda (stx)
        (letrec* ((rel-tol-default 1e-12)
                  (abs-tol-default 1e-12)
                  (parse-opts
                   (lambda (options)
                     (let loop
                       ((remaining options)
                        (rel-tol rel-tol-default)
                        (abs-tol abs-tol-default))
                       (cond ((null? remaining) (cons rel-tol abs-tol))
                             ((null? (cdr remaining))
                              (error "check-approx option requires a value" (car remaining)))
                             ((equal? (car remaining) :rel-tol)
                              (loop (cddr remaining) (cadr remaining) abs-tol))
                             ((equal? (car remaining) :abs-tol)
                              (loop (cddr remaining) rel-tol (cadr remaining)))
                             (else (error "check-approx unrecognized option" (car remaining)))))))
                  (build
                   (lambda (stx expr-datum expected-datum opts)
                     (let* ((parsed (parse-opts opts))
                            (rel-tol (car parsed))
                            (abs-tol (cdr parsed)))
                       (datum->syntax stx
                         `(check:proc (quote ,expr-datum)
                            (lambda () ,expr-datum)
                            ,expected-datum
                            (lambda (actual expected)
                              (and (number? actual) (number? expected)
                                (number? ,rel-tol) (number? ,abs-tol)
                                (or (= actual expected)
                                  (let* ((difference (abs (- actual expected)))
                                         (relative-tolerance (abs ,rel-tol))
                                         (absolute-tolerance (abs ,abs-tol))
                                         (scale (max (abs actual) (abs expected)))
                                         (limit (max absolute-tolerance (* relative-tolerance scale))))
                                    (<= difference limit)))))))))))
          (syntax-case stx (=>)
            ((_ expr => expected opts ...)
             (build stx (syntax->datum #'expr)
                    (syntax->datum #'expected)
                    (syntax->datum #'(opts ...))))
            ((_ expr expected opts ...)
             (build stx (syntax->datum #'expr)
                    (syntax->datum #'expected)
                    (syntax->datum #'(opts ...))))))))

    (define-syntax check-catch
      (syntax-rules ()
        ((check-catch error-id body)
         (check (catch error-id (lambda () body) (lambda args error-id))
           => error-id))))

    (define-syntax test
      (syntax-rules ()
        ((test left right) (check left => right))))

    (define (check-report . msg)
      (if (not (null? msg)) (begin (display (car msg))))
      (srfi-78-check-report)
      (if (check-failed?) (exit -1))
    ) ;define
  ) ;begin
) ;define-library
