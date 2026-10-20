;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension registry prompt)
  (import (scheme base)
    (liii error)
    (liii pi extension types)
    (liii pi extension registry base)
  ) ;import
  (export register-prompt-injector! extension-register-prompt-injector!
    extension-registry-prompt-injectors-list
    extension-registry-collect-prompt-injectors
  ) ;export
  (begin

    (define (register-prompt-injector! target injector)
      (let ((reg (target->registry target)))
        (unless (or (string? injector) (procedure? injector))
          (type-error "register-prompt-injector!: injector must be a string or procedure")
        ) ;unless
        (set-extension-registry-prompt-injectors! reg
          (append (extension-registry-prompt-injectors reg) (list injector))
        ) ;set-extension-registry-prompt-injectors!
        reg
      ) ;let
    ) ;define

    (define extension-register-prompt-injector! register-prompt-injector!)

    (define (extension-registry-prompt-injectors-list target)
      (let ((reg (target->registry target)))
        (extension-registry-prompt-injectors reg)
      ) ;let
    ) ;define

    (define (%short-err-msg args)
      (if (null? args)
        ""
        (let* ((first (car args))
               (raw (if (string? first) first (object->string first)))
               (clean (if (> (string-length raw) 120) (string-append (substring raw 0 120) "...") raw)
               ) ;clean
              ) ;
          clean
        ) ;let*
      ) ;if
    ) ;define

    (define (%log-injector-error k . args)
      (let ((err-p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display "[WARN] prompt-injector error: " err-p)
        (display k err-p)
        (display ": " err-p)
        (display (%short-err-msg args) err-p)
        (newline err-p)
      ) ;let
      #f
    ) ;define

    (define (%call-injector proc tool-calls reg)
      (let ((ar (car (arity proc))))
        (cond ((= ar 0) (proc))
              ((= ar 1) (proc tool-calls))
              (else (proc tool-calls reg))
        ) ;cond
      ) ;let
    ) ;define

    (define (%eval-injector inj tool-calls reg)
      (cond ((string? inj) inj)
            ((procedure? inj)
             (catch #t (lambda () (%call-injector inj tool-calls reg)) %log-injector-error)
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define* (extension-registry-collect-prompt-injectors target (tool-calls '()))
      (let* ((reg (target->registry target))
             (injectors (extension-registry-prompt-injectors reg))
            ) ;
        (let loop
          ((rest injectors) (acc '()))
          (if (null? rest)
            (reverse acc)
            (let ((val (%eval-injector (car rest) tool-calls reg)))
              (if (and (string? val) (> (string-length val) 0))
                (loop (cdr rest) (cons val acc))
                (loop (cdr rest) acc)
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
