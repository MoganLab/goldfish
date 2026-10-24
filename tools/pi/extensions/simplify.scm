;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(import (scheme base)
  (liii string)
  (liii pi code extension)
  (liii pi code args query)
  (liii pi simplify modes)
) ;import

(define (simplify-extension api . opt-context)
  ;; 1. 注册 flags
  (register-flag! api
    (make-extension-flag-def "--simplify" "Simplify code" :key 'simplify)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--apply" "Apply suggestions" :key 'apply)
  ) ;register-flag!

  ;; 2. 注册 commands
  (register-command! api
    (make-extension-command-def "/simplify"
      "Simplify code changes (/simplify)"
      (lambda (args ctx) "simplify command executed")
      :params '()
    ) ;make-extension-command-def
  ) ;register-command!
  (register-command! api
    (make-extension-command-def "simplify"
      "Simplify code changes (simplify)"
      (lambda (args ctx) "simplify command executed")
      :params '()
    ) ;make-extension-command-def
  ) ;register-command!

  ;; 3. 注册 validator
  (register-validator! api
    (lambda (parsed . extra)
      (let ((simplify (pi-args-ref parsed 'simplify))
            (apply? (pi-args-ref parsed 'apply))
            (code-review (pi-args-ref parsed 'code-review))
           ) ;
        (when (and simplify code-review)
          (error 'pi-arg-error "--code-review and --simplify cannot be used together")
        ) ;when
        (when (and apply? (not simplify))
          (error 'pi-arg-error "--apply requires --simplify")
        ) ;when
      ) ;let
    ) ;lambda
  ) ;register-validator!

  ;; 4. 注册 flow
  (register-flow! api
    (make-extension-flow-def 'simplify
      (lambda (p) (pi-args-ref p 'simplify))
      (lambda (parsed cfg mode initial-msg) (run-simplify-flow parsed cfg mode))
      :local?   #f
      :priority 10
    ) ;make-extension-flow-def
  ) ;register-flow!

  "simplify-loaded"
) ;define

simplify-extension
