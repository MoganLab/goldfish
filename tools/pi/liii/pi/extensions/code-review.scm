;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(import (scheme base)
  (liii string)
  (liii pi extension)
  (liii pi code-review)
  (liii pi modes review)
) ;import

(define (%ref parsed key)
  (let ((p (assoc key parsed)))
    (if p (cdr p) #f)
  ) ;let
) ;define

(define (code-review-extension api . opt-context)
  ;; 1. 注册 flags
  (register-flag! api
    (make-extension-flag-def "--code-review" "Review diff" :key 'code-review)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--auto-review" "Auto review" :key 'auto-review)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--staged" "Review staged diff" :key 'staged)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--ref"
      "Review git ref"
      :key        'review-ref
      :takes-arg? #t
    ) ;make-extension-flag-def
  ) ;register-flag!

  ;; 2. 注册 commands
  (register-command! api
    (make-extension-command-def "/review"
      "Run code review on changes (/review)"
      (lambda (args ctx) "code-review command executed")
      :params '()
    ) ;make-extension-command-def
  ) ;register-command!
  (register-command! api
    (make-extension-command-def "review"
      "Run code review on changes (review)"
      (lambda (args ctx) "code-review command executed")
      :params '()
    ) ;make-extension-command-def
  ) ;register-command!
  (register-command! api
    (make-extension-command-def "/code-review"
      "Run code review on changes (/code-review)"
      (lambda (args ctx) "code-review command executed")
      :params '()
    ) ;make-extension-command-def
  ) ;register-command!
  (register-command! api
    (make-extension-command-def "code-review"
      "Run code review on changes (code-review)"
      (lambda (args ctx) "code-review command executed")
      :params '()
    ) ;make-extension-command-def
  ) ;register-command!

  ;; 3. 注册 validator
  (register-validator! api
    (lambda (parsed . extra)
      (let ((code-review (%ref parsed 'code-review))
            (auto-review (%ref parsed 'auto-review))
            (simplify (%ref parsed 'simplify))
            (staged (%ref parsed 'staged))
            (ref (%ref parsed 'review-ref))
           ) ;
        (when (and code-review auto-review)
          (error 'pi-arg-error "--code-review and --auto-review cannot be used together")
        ) ;when
        (when (and code-review simplify)
          (error 'pi-arg-error "--code-review and --simplify cannot be used together")
        ) ;when
        (when (and staged (not (or code-review simplify)))
          (error 'pi-arg-error "--staged requires --code-review or --simplify")
        ) ;when
        (when (and ref (not (or code-review simplify)))
          (error 'pi-arg-error "--ref requires --code-review or --simplify")
        ) ;when
      ) ;let
    ) ;lambda
  ) ;register-validator!

  ;; 4. 注册 flow
  (register-flow! api
    (make-extension-flow-def 'code-review
      (lambda (p) (%ref p 'code-review))
      (lambda (parsed cfg mode initial-msg) (run-code-review-flow parsed cfg mode))
      :local?   #f
      :priority 10
    ) ;make-extension-flow-def
  ) ;register-flow!

  ;; 5. 注册 prompt injector
  (register-prompt-injector! api
    (lambda (tool-calls)
      (let ((block (pi-build-review-injection (pi-edited-files-from-tool-calls tool-calls)))
           ) ;
        (if block (string-trim-both block) #f)
      ) ;let
    ) ;lambda
  ) ;register-prompt-injector!

  "code-review-loaded"
) ;define

code-review-extension
