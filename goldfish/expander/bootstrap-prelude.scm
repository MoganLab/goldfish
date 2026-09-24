;;; Bootstrap-only placeholders for mutually recursive prelude transformers.
;;; The real definitions in liii/prelude.scm replace these immediately.

(define-syntax and
  (lambda (stx) (datum->syntax stx '(if #t #t #f))))
(define-syntax or
  (lambda (stx) (datum->syntax stx '(if #t #t #f))))
(define-syntax cond
  (lambda (stx) (datum->syntax stx '(if #t #t #f))))
(define-syntax case
  (lambda (stx) (datum->syntax stx '(if #t #t #f))))
