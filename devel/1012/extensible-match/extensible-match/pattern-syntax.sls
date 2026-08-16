(library (extensible-match pattern-syntax)
  (export define-pattern-syntax
          expand-pattern-syntax)
  (import (rnrs (6))
          (srfi :213 #;identifier-properties))

  ;; Identifier property key used to hold match transformer procedures
  (define-syntax pattern-transformer (syntax-rules ()))

    ;; Public interface to define transformers for pattern syntax
  (define-syntax define-pattern-syntax
    (lambda (stx)
      (syntax-case stx ()
        ((_ for-id proc)
         #'(define-property for-id pattern-transformer
             (let ((tx proc))
               (if (procedure? tx)
                   tx
                   (assertion-violation 'define-pattern-syntax
                                        "pattern transformer must be a procedure"
                                        tx))))))))

  (define-syntax expand-pattern-syntax
    (lambda (stx)
      (capture-lookup
       (lambda (lookup)
         (syntax-case stx ()
           ((_ (patstx-keyword . rest) k-keyword . k-subforms)
            (let ((tx (lookup #'patstx-keyword #'pattern-transformer)))
              (if tx
                  #`(k-keyword #,(tx #'(patstx-keyword . rest)) . k-subforms)
                  (syntax-violation 'match
                                    "pattern syntax not defined"
                                    #'patstx-keyword))))))))))
