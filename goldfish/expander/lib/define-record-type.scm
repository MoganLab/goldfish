;;; define-record-type.scm
;;; define-record-type as an ordinary object-level macro (cf. the host
;;; define-macro in prelude.scm / the previous host procedural transformer
;;; in boot/primitives.scm).  It expands to inlet-based predicate /
;;; constructor / accessors, using the same representation as the host so
;;; records interoperate.  Being part of the self-hosted macro layer (like
;;; syntax-case / syntax-rules), it is expanded by the expander itself.

(define (dr-construct-inlet-args make-params fields)
  (apply append
         (map (lambda (field)
                (let* ((fd (syntax->datum field))
                       (f (car fd))
                       (par (memq f make-params)))
                  (list (list 'quote f)
                        (if (pair? par) f #f))))
              fields)))

(define (dr-accessor-defs fields)
  (apply append
         (map (lambda (field)
                (let* ((fd (syntax->datum field))
                       (f (car fd))
                       (acc (cadr fd))
                       (acc-def (list 'define (list acc 'obj)
                                      (list 'let-ref 'obj (list 'quote f))))
                       (mod (if (pair? (cddr fd)) (caddr fd) #f)))
                  (if mod
                      (list acc-def
                            (list 'define (list mod 'obj 'val)
                                  (list 'let-set! 'obj (list 'quote f) 'val)))
                      (list acc-def))))
              fields)))

(define-syntax define-record-type
  (lambda (stx)
    (let* ((form (syntax-form stx))
           (type (syntax->datum (cadr form)))
           (make-datum (syntax->datum (caddr form)))
           (make-params (cdr make-datum))
           (pred (cadddr form))
           (fields (cddddr form)))
      (datum->syntax stx
        (append
         (list 'begin
               (list 'define (list pred 'obj)
                     (list 'and (list 'let? 'obj)
                           (list 'eq? (list 'let-ref 'obj (list 'quote type))
                                 (list 'quote type))))
               (list 'define make-datum
                     (append (list 'inlet (list 'quote type) (list 'quote type))
                             (dr-construct-inlet-args make-params fields))))
         (dr-accessor-defs fields))))))