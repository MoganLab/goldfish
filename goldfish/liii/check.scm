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

    (define default-check-approx-rel-tol 1e-12)
    (define default-check-approx-abs-tol 1e-12)

    (define (parse-check-approx-options options)
      (let loop
        ((remaining options)
         (rel-tol default-check-approx-rel-tol)
         (abs-tol default-check-approx-abs-tol)
        ) ;
        (cond ((null? remaining) (cons rel-tol abs-tol))
              ((null? (cdr remaining))
               (error "check-approx option requires a value" (car remaining))
              ) ;
              ((equal? (car remaining) :rel-tol)
               (loop (cddr remaining) (cadr remaining) abs-tol)
              ) ;
              ((equal? (car remaining) :abs-tol)
               (loop (cddr remaining) rel-tol (cadr remaining))
              ) ;
              (else (error "check-approx unrecognized option" (car remaining)))
        ) ;cond
      ) ;let
    ) ;define

    (define (build-check-approx stx expr-datum expected-datum opts)
      (let* ((parsed (parse-check-approx-options opts))
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
                     (<= difference limit)))))))))

    ;; (check-approx expr => expected opts ...) is the documented form
    ;; (matching srfi-78's check); the arrowless form stays accepted.
    (define-syntax check-approx
      (lambda (stx)
        (syntax-case stx (=>)
          ((_ expr => expected opts ...)
           (build-check-approx stx (syntax->datum #'expr)
                               (syntax->datum #'expected)
                               (syntax->datum #'(opts ...))))
          ((_ expr expected opts ...)
           (build-check-approx stx (syntax->datum #'expr)
                               (syntax->datum #'expected)
                               (syntax->datum #'(opts ...)))))))

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
