;;; define-record-type.scm
;;; define-record-type as an ordinary object-level macro (cf. the host
;;; define-macro in seed/prelude).  It expands to vector-layout records:
;;; a type descriptor (make-record-type) plus constructor / predicate /
;;; accessors / modifiers, all backed by the independent record
;;; implementation in liii/boot.scm (Guile-style, eq? type identity).
;;; This is the self-hosted macro layer (like syntax-case / syntax-rules),
;;; expanded by the expander itself.

(define (dr-field-datum field)
  (syntax->datum field))

(define (dr-record-defs type make-datum pred fields)
  (let ((rtd (next-record-rtd))
        (make-name (car make-datum))
        (make-params (cdr make-datum))
        (field-names (map car fields))
        (acc-defs
          (let loop ((fs fields) (i 1))
            (if (null? fs)
              '()
              (let ((acc (cadr (car fs))))
                (cons (list 'define (list acc 'obj) (list 'vector-ref 'obj i))
                      (if (pair? (cddr (car fs)))
                        (let ((mod (caddr (car fs))))
                          (cons (list 'define (list mod 'obj 'val)
                                      (list 'vector-set! 'obj i 'val))
                                (loop (cdr fs) (+ i 1))))
                        (loop (cdr fs) (+ i 1)))))))))
    (append
     (list 'begin
           (list 'define rtd
                 (list 'make-record-type (list 'quote type) (list 'quote field-names)))
           (list 'define make-datum
                 (cons 'vector (cons rtd make-params)))
           (list 'define (list pred 'obj)
                 (list 'and
                       (list 'vector? 'obj)
                       (list 'positive? (list 'vector-length 'obj))
                       (list 'eq? (list 'vector-ref 'obj 0) rtd))))
     acc-defs)))

(define-syntax define-record-type
  (lambda (stx)
    (let* ((form (syntax-form stx))
           (type (syntax->datum (cadr form)))
           (make-datum (syntax->datum (caddr form)))
           (pred (cadddr form))
           (fields (map dr-field-datum (cddddr form))))
      (datum->syntax stx
        (dr-record-defs type make-datum pred fields)))))
