;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core extensions flow-def)
  (import (scheme base) (liii base) (liii error))
  (export make-extension-flow-def extension-flow-def? extension-flow-def-name
    extension-flow-def-predicate extension-flow-def-handler
    extension-flow-def-local? extension-flow-def-priority
  ) ;export
  (begin

    (define*-record-type <extension-flow-def>
      (make-extension-flow-def name predicate handler (local? #f) (priority 0))
      extension-flow-def?
      (name extension-flow-def-name)
      (predicate extension-flow-def-predicate)
      (handler extension-flow-def-handler)
      (local? extension-flow-def-local?)
      (priority extension-flow-def-priority)
    ) ;define*-record-type

    (let ((raw make-extension-flow-def))
      (set! make-extension-flow-def
        (lambda* (name predicate handler (local? #f) (priority 0))
          (unless (or (string? name) (symbol? name))
            (type-error "make-extension-flow-def: name must be a string or symbol")
          ) ;unless
          (unless (procedure? predicate)
            (type-error "make-extension-flow-def: predicate must be a procedure")
          ) ;unless
          (unless (procedure? handler)
            (type-error "make-extension-flow-def: handler must be a procedure")
          ) ;unless
          (raw name
            predicate
            handler
            (if local? #t #f)
            (if (number? priority) priority 0)
          ) ;raw
        ) ;lambda*
      ) ;set!
    ) ;let

  ) ;begin
) ;define-library
