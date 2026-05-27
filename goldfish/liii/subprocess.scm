;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reserved.
;;

(define-library (liii subprocess)
  (export run
    run-string
    run-strings
  ) ;export
  (import (scheme base) (liii base) (liii error) (liii string))
  (begin

    (define (subprocess-parse-args rest)
      (let loop ((args '()) (opts '()) (lst rest))
        (cond ((null? lst)
               (values (reverse args) (reverse opts)))
              ((keyword? (car lst))
               (if (null? (cdr lst))
                 (error 'syntax-error "missing value for keyword" (car lst))
                 (loop args
                       (cons (cons (keyword->symbol (car lst)) (cadr lst)) opts)
                       (cddr lst))))
              (else
               (loop (cons (car lst) args) opts (cdr lst))))
      ) ;let loop
    ) ;define

    (define (string-split-lines str)
      (if (string-null? str)
        '()
        (map (lambda (line)
               (if (and (> (string-length line) 0)
                        (char=? #\return (string-ref line (- (string-length line) 1))))
                 (substring line 0 (- (string-length line) 1))
                 line))
             (string-split str #\newline))
      ) ;if
    ) ;define

    (define (run cmd . rest)
      (let-values (((args opts) (subprocess-parse-args rest)))
        (let-values (((code out err) (g_subprocess-run cmd args opts)))
          code))
    ) ;define

    (define (run-string cmd . rest)
      (let-values (((args opts) (subprocess-parse-args rest)))
        (let-values (((code out err) (g_subprocess-run cmd args (cons '(output . string) opts))))
          out))
    ) ;define

    (define (run-strings cmd . rest)
      (let-values (((args opts) (subprocess-parse-args rest)))
        (let* ((opts (cons '(output . string) opts))
               (has-stderr? (assoc 'stderr opts))
               (opts (if has-stderr? opts (cons '(stderr . string) opts)))
               (stderr-mode (cdr (assoc 'stderr opts)))
              ) ;let*
          (let-values (((code out err) (g_subprocess-run cmd args opts)))
            (values (string-split-lines out)
                    (if (eq? stderr-mode 'string) (string-split-lines err) '())
                    code))
        ) ;let*
      ) ;let-values
    ) ;define

  ) ;begin
) ;define-library
