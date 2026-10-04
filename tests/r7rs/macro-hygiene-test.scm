(import (scheme base) (liii check))
(check-set-mode! 'report-failed)

;; 4.3: introduced binders and definition-site references avoid use-site capture.
(define-syntax hygienic-or
  (syntax-rules ()
    ((_ a b) (let ((temporary a)) (if temporary temporary b)))))
(let ((temporary 99) (let #f) (if #f))
  (check (hygienic-or #f temporary) => 99))
(let ((helper (lambda (x) (+ x 1))))
  (let-syntax ((increment (syntax-rules () ((_ x) (helper x)))))
    (let ((helper (lambda (x) 0)))
      (check (increment 41) => 42))))

;; Literal identifiers compare bindings, including the unbound case.
(define-syntax literal-binding
  (syntax-rules (marker)
    ((_ marker) 'matched)
    ((_ x) 'different)))
(check (literal-binding marker) => 'matched)
(let ((marker 42)) (check (literal-binding marker) => 'different))

;; let-syntax specifications see the enclosing scope; letrec-syntax is recursive.
(let-syntax ((helper (syntax-rules () ((_ x) (+ x 1)))))
  (let-syntax ((helper (syntax-rules () ((_ x) (+ x 100))))
               (caller (syntax-rules () ((_ x) (helper x)))))
    (check (caller 41) => 42)))
(letrec-syntax ((helper (syntax-rules () ((_ x) (+ x 1))))
                (caller (syntax-rules () ((_ x) (helper x)))))
  (check (caller 41) => 42))

;; Nested, vector, dotted, and custom-ellipsis patterns cover structural matching.
(define-syntax nested
  (syntax-rules () ((_ ((x ...) ...)) (list (list x ...) ...))))
(check (nested ((1 2) () (3))) => '((1 2) () (3)))
(define-syntax vector-tail
  (syntax-rules () ((_ #(x ...) . rest) (list (list x ...) 'rest))))
(check (vector-tail #(1 2) . tail) => '((1 2) tail))
(define-syntax custom
  (syntax-rules ::: () ((_ x :::) (list x :::))))
(check (custom 1 2 3) => '(1 2 3))

;; A macro-generated internal definition binds the supplied use-site identifier.
(let ()
  (define-syntax define-answer
    (syntax-rules () ((_ name) (define name 42))))
  (define-answer answer)
  (check answer => 42))

(check-report)
