;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension builtin flags)
  (import (scheme base)
    (liii string)
    (liii pi extension types)
    (liii pi extension registry)
    (liii pi blueprint args)
    (liii pi compass args)
  ) ;import
  (export register-builtin-flags!)
  (begin

    (define (%ref parsed key)
      (let ((p (assoc key parsed)))
        (if p (cdr p) #f)
      ) ;let
    ) ;define

    (define (%register-blueprint-flags! reg)
      (register-flag! reg
        (make-extension-flag-def "--blueprint" "Blueprint goal" :key 'blueprint)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--plan-status" "Plan status" :key 'plan-status)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--plan-next" "Plan next task" :key 'plan-next)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--plan-verify" "Plan verify" :key 'plan-verify)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--plan-abandon" "Plan abandon" :key 'plan-abandon)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--approve" "Approve plan gate" :key 'approve)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--blueprint-file"
          "Blueprint plan file"
          :key        'blueprint-file
          :takes-arg? #t
        ) ;make-extension-flag-def
      ) ;register-flag!
      (register-validator! reg
        (lambda (parsed . extra)
          (pi-blueprint-validate parsed)
          (let ((msg (and (pair? extra) (car extra)))
                (rest (and (pair? extra) (pair? (cdr extra)) (cadr extra)))
               ) ;
            (when (and (%ref parsed 'blueprint)
                    (string? msg)
                    (string-null? msg)
                    (or (not rest) (null? rest))
                  ) ;and
              (error 'pi-arg-error
                "--blueprint requires an objective (message text or @file)"
              ) ;error
            ) ;when
          ) ;let
        ) ;lambda
      ) ;register-validator!
    ) ;define

    (define (%register-compass-flags! reg)
      (register-flag! reg
        (make-extension-flag-def "--codemap" "Generate codemap" :key 'codemap)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--refresh" "Refresh codemap" :key 'refresh)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--no-codemap-inject"
          "Disable injection"
          :key 'no-codemap-inject
        ) ;make-extension-flag-def
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--codemap-file"
          "Codemap path"
          :key        'codemap-file
          :takes-arg? #t
        ) ;make-extension-flag-def
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--tour"
          "Code tour"
          :key        'tour
          :takes-arg? #t
          :handler
          (lambda (arg next-arg)
            (if (and next-arg
                  (not (string-starts? next-arg "-"))
                  (not (string-starts? next-arg "@"))
                ) ;and
              (values next-arg 2)
              (values #t 1)
            ) ;if
          ) ;lambda
        ) ;make-extension-flag-def
      ) ;register-flag!
      (register-validator! reg (lambda (parsed . extra) (pi-compass-validate parsed)))
    ) ;define

    (define (%register-review-flags! reg)
      (register-flag! reg
        (make-extension-flag-def "--code-review" "Review diff" :key 'code-review)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--auto-review" "Auto review" :key 'auto-review)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--simplify" "Simplify code" :key 'simplify)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--red-green" "TDD mode" :key 'red-green)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--apply" "Apply suggestions" :key 'apply)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--staged" "Review staged diff" :key 'staged)
      ) ;register-flag!
      (register-flag! reg
        (make-extension-flag-def "--ref"
          "Review git ref"
          :key        'review-ref
          :takes-arg? #t
        ) ;make-extension-flag-def
      ) ;register-flag!
      (register-validator! reg
        (lambda (parsed . extra)
          (let ((code-review (%ref parsed 'code-review))
                (auto-review (%ref parsed 'auto-review))
                (simplify (%ref parsed 'simplify))
                (red-green (%ref parsed 'red-green))
                (apply? (%ref parsed 'apply))
                (staged (%ref parsed 'staged))
                (ref (%ref parsed 'review-ref))
                (msg (and (pair? extra) (car extra)))
                (rest (and (pair? extra) (pair? (cdr extra)) (cadr extra)))
               ) ;
            (when (and code-review auto-review)
              (error 'pi-arg-error "--code-review and --auto-review cannot be used together")
            ) ;when
            (when (and code-review simplify)
              (error 'pi-arg-error "--code-review and --simplify cannot be used together")
            ) ;when
            (when (and red-green (or code-review simplify))
              (error 'pi-arg-error
                "--red-green cannot be used with --code-review or --simplify"
              ) ;error
            ) ;when
            (when (and red-green (or apply? staged ref))
              (error 'pi-arg-error
                "--apply, --staged and --ref are not supported with --red-green"
              ) ;error
            ) ;when
            (when (and apply? (not simplify))
              (error 'pi-arg-error "--apply requires --simplify")
            ) ;when
            (when (and staged (not (or code-review simplify)))
              (error 'pi-arg-error "--staged requires --code-review or --simplify")
            ) ;when
            (when (and ref (not (or code-review simplify)))
              (error 'pi-arg-error "--ref requires --code-review or --simplify")
            ) ;when
            (when (and red-green (string? msg) (string-null? msg) (or (not rest) (null? rest)))
              (error 'pi-arg-error
                "--red-green requires a task description (message text or @file)"
              ) ;error
            ) ;when
          ) ;let
        ) ;lambda
      ) ;register-validator!
    ) ;define

    (define (register-builtin-flags! reg)
      (%register-blueprint-flags! reg)
      (%register-compass-flags! reg)
      (%register-review-flags! reg)
      reg
    ) ;define

    (add-registry-initializer! register-builtin-flags!)

  ) ;begin
) ;define-library
