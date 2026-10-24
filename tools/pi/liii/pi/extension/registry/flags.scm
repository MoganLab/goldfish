;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension registry flags)
  (import (scheme base)
    (liii error)
    (liii list)
    (liii pi extension types)
    (liii pi extension registry base)
  ) ;import
  (export normalize-flag-def register-flag! extension-register-flag!
    find-extension-flag extension-registry-flags-list
  ) ;export
  (begin

    (define (normalize-flag-def def)
      (cond ((extension-flag-def? def) def)
            ((pair? def)
             (let ((flag (cond ((assq 'flag def) => cdr) (else #f)))
                   (desc (cond ((assq 'description def) => cdr) (else #f)))
                   (handler (cond ((assq 'handler def) => cdr) (else #f)))
                   (type (cond ((assq 'type def) => cdr) (else 'boolean)))
                   (default (cond ((assq 'default def) => cdr) (else #f)))
                   (key (cond ((assq 'key def) => cdr) (else #f)))
                   (aliases (cond ((assq 'aliases def) => cdr) (else '())))
                   (takes-arg? (cond ((assq 'takes-arg? def) => cdr) (else #f)))
                   (validator (cond ((assq 'validator def) => cdr) (else #f)))
                  ) ;
               (make-extension-flag-def flag
                 desc
                 :handler    handler
                 :type       type
                 :default    default
                 :key        key
                 :aliases    aliases
                 :takes-arg? takes-arg?
                 :validator  validator
               ) ;make-extension-flag-def
             ) ;let
            ) ;
            (else (type-error "flag-def must be an extension-flag-def or alist"))
      ) ;cond
    ) ;define

    (define (register-flag! target flag-def)
      (let* ((reg (target->registry target))
             (flag (normalize-flag-def flag-def))
             (flags (extension-registry-flags reg))
             (updated (upsert-by-key flags extension-flag-def-flag flag))
            ) ;
        (set-extension-registry-flags! reg updated)
        flag
      ) ;let*
    ) ;define

    (define extension-register-flag! register-flag!)

    (define (find-extension-flag target flag-name)
      (let* ((reg (target->registry target)) (flags (extension-registry-flags reg)))
        (find
          (lambda (f)
            (or (string=? (extension-flag-def-flag f) flag-name)
              (member flag-name (extension-flag-def-aliases f))
            ) ;or
          ) ;lambda
          flags
        ) ;find
      ) ;let*
    ) ;define

    (define (extension-registry-flags-list target)
      (extension-registry-flags (target->registry target))
    ) ;define

  ) ;begin
) ;define-library
