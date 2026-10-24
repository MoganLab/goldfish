;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code extension registry flows)
  (import (scheme base)
    (liii error)
    (liii list)
    (liii pi code extension types)
    (liii pi code extension registry base)
  ) ;import
  (export normalize-flow-def register-flow! extension-register-flow!
    find-extension-flow extension-registry-flows-list register-validator!
    extension-register-validator! extension-registry-validators-list
    register-config-augmenter! extension-registry-augmenters-list
  ) ;export
  (begin

    (define (normalize-flow-def def)
      (cond ((extension-flow-def? def) def)
            ((pair? def)
             (let ((name (cond ((assq 'name def) => cdr) (else #f)))
                   (pred (cond ((assq 'predicate def) => cdr) (else #f)))
                   (handler (cond ((assq 'handler def) => cdr) (else #f)))
                   (local? (cond ((assq 'local? def) => cdr) (else #f)))
                   (priority (cond ((assq 'priority def) => cdr) (else 0)))
                  ) ;
               (make-extension-flow-def name pred handler :local? local? :priority priority)
             ) ;let
            ) ;
            (else (type-error "flow-def must be an extension-flow-def or alist"))
      ) ;cond
    ) ;define

    (define (%flow-key-str flow)
      (let ((name (extension-flow-def-name flow)))
        (if (symbol? name) (symbol->string name) (object->string name))
      ) ;let
    ) ;define

    (define (insert-flow-by-priority lst new-flow)
      (let ((key (%flow-key-str new-flow)) (prio (extension-flow-def-priority new-flow)))
        (let loop
          ((rest lst) (acc '()))
          (cond ((null? rest) (reverse (cons new-flow acc)))
                ((string=? (%flow-key-str (car rest)) key)
                 (append (reverse acc) (cons new-flow (cdr rest)))
                ) ;
                ((< (extension-flow-def-priority (car rest)) prio)
                 (append (reverse acc) (cons new-flow rest))
                ) ;
                (else (loop (cdr rest) (cons (car rest) acc)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (register-flow! target flow-def)
      (let* ((reg (target->registry target))
             (flow (normalize-flow-def flow-def))
             (flows (extension-registry-flows reg))
             (updated (insert-flow-by-priority flows flow))
            ) ;
        (set-extension-registry-flows! reg updated)
        flow
      ) ;let*
    ) ;define

    (define extension-register-flow! register-flow!)

    (define (find-extension-flow target name)
      (let* ((reg (target->registry target))
             (flows (extension-registry-flows reg))
             (target-str (if (symbol? name) (symbol->string name) (object->string name)))
            ) ;
        (find (lambda (f) (string=? (%flow-key-str f) target-str)) flows)
      ) ;let*
    ) ;define

    (define (extension-registry-flows-list target)
      (extension-registry-flows (target->registry target))
    ) ;define

    ;; ----------------- Validators -----------------

    (define (register-validator! target validator-proc)
      (unless (procedure? validator-proc)
        (type-error "validator must be a procedure")
      ) ;unless
      (let* ((reg (target->registry target))
             (validators (extension-registry-validators reg))
             (updated (append validators (list validator-proc)))
            ) ;
        (set-extension-registry-validators! reg updated)
        validator-proc
      ) ;let*
    ) ;define

    (define extension-register-validator! register-validator!)

    (define (extension-registry-validators-list target)
      (extension-registry-validators (target->registry target))
    ) ;define

    ;; ----------------- Config Augmenters -----------------

    (define (register-config-augmenter! target proc)
      (unless (procedure? proc)
        (type-error "config-augmenter must be a procedure")
      ) ;unless
      (let* ((reg (target->registry target))
             (augmenters (extension-registry-augmenters reg))
             (updated (append augmenters (list proc)))
            ) ;
        (set-extension-registry-augmenters! reg updated)
        proc
      ) ;let*
    ) ;define

    (define (extension-registry-augmenters-list target)
      (extension-registry-augmenters (target->registry target))
    ) ;define

  ) ;begin
) ;define-library
