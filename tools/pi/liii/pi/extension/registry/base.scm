;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension registry base)
  (import (scheme base) (liii error) (liii pi extension types))
  (export target->registry upsert-by-key extension-registry-get-listeners
    current-extension-registry set-current-extension-registry!
    with-extension-registry default-extension-registry
    set-default-extension-registry! set-registry-initializer!
    add-registry-initializer!
  ) ;export
  (begin

    (define (target->registry target)
      (cond ((extension-registry? target) target)
            ((extension-api? target) (extension-api-registry target))
            ((extension-context? target) (extension-context-registry target))
            (else (type-error "target must be an extension-api, extension-context, or extension-registry"
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    (define (upsert-by-key lst key-fn new-item)
      (let loop
        ((rest lst) (acc '()) (replaced #f))
        (if (null? rest)
          (if replaced (reverse acc) (reverse (cons new-item acc)))
          (let ((item (car rest)))
            (if (string=? (key-fn item) (key-fn new-item))
              (loop (cdr rest) (cons new-item acc) #t)
              (loop (cdr rest) (cons item acc) replaced)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (extension-registry-get-listeners target event-type)
      (let* ((reg (target->registry target))
             (evt-sym (cond ((symbol? event-type) event-type)
                            ((string? event-type) (string->symbol event-type))
                            (else (type-error "event-type must be a symbol or string"))
                      ) ;cond
             ) ;evt-sym
             (entry (assq evt-sym (extension-registry-listeners reg)))
            ) ;
        (if entry (cdr entry) '())
      ) ;let*
    ) ;define

    ;; ----------------- Active & Default Registry -----------------

    (define *current-extension-registry* #f)
    (define *default-extension-registry* #f)
    (define *registry-initializers* '())

    (define (add-registry-initializer! proc)
      (set! *registry-initializers* (append *registry-initializers* (list proc)))
      (when *default-extension-registry*
        (proc *default-extension-registry*)
      ) ;when
    ) ;define

    (define set-registry-initializer! add-registry-initializer!)

    (define (current-extension-registry)
      *current-extension-registry*
    ) ;define

    (define (set-current-extension-registry! target)
      (if (not target)
        (begin
          (set! *current-extension-registry* #f)
          #f
        ) ;begin
        (let ((reg (target->registry target)))
          (set! *current-extension-registry* reg)
          reg
        ) ;let
      ) ;if
    ) ;define

    (define (with-extension-registry target thunk)
      (let ((prev *current-extension-registry*)
            (reg (if (not target) #f (target->registry target)))
           ) ;
        (dynamic-wind (lambda () (set! *current-extension-registry* reg))
          thunk
          (lambda () (set! *current-extension-registry* prev))
        ) ;dynamic-wind
      ) ;let
    ) ;define

    (define (default-extension-registry)
      (unless *default-extension-registry*
        (let ((reg (make-extension-registry)))
          (for-each (lambda (init-fn) (init-fn reg)) *registry-initializers*)
          (set! *default-extension-registry* reg)
        ) ;let
      ) ;unless
      *default-extension-registry*
    ) ;define

    (define (set-default-extension-registry! target)
      (if (not target)
        (set! *default-extension-registry* #f)
        (set! *default-extension-registry* (target->registry target))
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
