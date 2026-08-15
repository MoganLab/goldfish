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
  (let ((rtd (gensym))
        (make-name (car make-datum))
        (make-params (cdr make-datum))
        (field-names (map car fields)))
    (append
     (list 'begin
           (list 'define rtd
                 (list 'make-record-type (list 'quote type) (list 'quote field-names)))
           (list 'define make-datum
                 (cons 'vector (cons rtd make-params)))
           (list 'define (list pred 'obj)
                 (list (list 'record-predicate rtd) 'obj)))
     (apply append
            (map (lambda (fd)
                   (let ((acc (cadr fd))
                         (mod (if (pair? (cddr fd)) (caddr fd) #f)))
                     (cons (list 'define (list acc 'obj)
                                 (list (list 'record-accessor rtd (list 'quote (car fd))) 'obj))
                           (if mod
                               (list (list 'define (list mod 'obj 'val)
                                           (list (list 'record-modifier rtd (list 'quote (car fd))) 'obj 'val)))
                               '()))))
                 fields)))))

(define-syntax define-record-type
  (lambda (stx)
    (let* ((form (syntax-form stx))
           (type (syntax->datum (cadr form)))
           (make-datum (syntax->datum (caddr form)))
           (pred (cadddr form))
           (fields (map dr-field-datum (cddddr form))))
      (datum->syntax stx
        (dr-record-defs type make-datum pred fields)))))
