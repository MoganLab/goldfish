;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(import (scheme base)
  (liii string)
  (liii pi code extension)
  (liii pi code args query)
  (liii pi code review)
  (liii pi code modes review)
) ;import

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
  (for-each
    (lambda (n)
      (register-command! api
        (make-extension-command-def n
          "Run code review on changes"
          (lambda (args ctx) "code-review command executed")
          :params '()
        ) ;make-extension-command-def
      ) ;register-command!
    ) ;lambda
    '("/review" "review" "/code-review" "code-review")
  ) ;for-each

  ;; 3. 注册 validator
  (register-validator! api
    (lambda (parsed . extra)
      (let ((code-review (pi-args-ref parsed 'code-review))
            (auto-review (pi-args-ref parsed 'auto-review))
            (simplify (pi-args-ref parsed 'simplify))
            (staged (pi-args-ref parsed 'staged))
            (ref (pi-args-ref parsed 'review-ref))
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
      (lambda (p) (pi-args-ref p 'code-review))
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
