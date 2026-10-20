;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define (%ref parsed key)
  (let ((p (assoc key parsed)))
    (if p (cdr p) #f)
  ) ;let
) ;define

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
      (let ((simplify (%ref parsed 'simplify))
            (apply? (%ref parsed 'apply))
            (code-review (%ref parsed 'code-review))
            (red-green (%ref parsed 'red-green))
           ) ;
        (when (and simplify code-review)
          (error 'pi-arg-error "--code-review and --simplify cannot be used together")
        ) ;when
        (when (and simplify red-green)
          (error 'pi-arg-error
            "--red-green cannot be used with --code-review or --simplify"
          ) ;error
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
      (lambda (p) (%ref p 'simplify))
      (lambda (parsed cfg mode initial-msg) (run-simplify-flow parsed cfg mode))
      :local?   #f
      :priority 10
    ) ;make-extension-flow-def
  ) ;register-flow!

  "simplify-loaded"
) ;define

simplify-extension
