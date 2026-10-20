;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(import (scheme base)
  (liii string)
  (liii pi extension)
  (liii pi blueprint args)
  (liii pi blueprint types)
  (liii pi blueprint runtime)
  (liii pi blueprint prompt)
  (liii pi blueprint tools)
  (liii pi modes blueprint)
  (liii pi modes blueprint common)
) ;import

(define (%ref parsed key)
  (let ((p (assoc key parsed)))
    (if p (cdr p) #f)
  ) ;let
) ;define

(define (blueprint-extension api . opt-context)
  ;; 1. 注册 flags
  (register-flag! api
    (make-extension-flag-def "--blueprint" "Blueprint goal" :key 'blueprint)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--plan-status" "Plan status" :key 'plan-status)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--plan-next" "Plan next task" :key 'plan-next)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--plan-verify" "Plan verify" :key 'plan-verify)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--plan-abandon" "Plan abandon" :key 'plan-abandon)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--approve" "Approve plan gate" :key 'approve)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--blueprint-file"
      "Blueprint plan file"
      :key        'blueprint-file
      :takes-arg? #t
    ) ;make-extension-flag-def
  ) ;register-flag!

  ;; 2. 注册 tools
  (register-tool! api (pi-blueprint-update-definition))
  (register-tool! api (pi-blueprint-status-definition))
  (register-tool! api (pi-blueprint-next-definition))

  ;; 3. 注册 validator
  (register-validator! api
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

  ;; 4. 注册 config augmenter (扩展 tool 白名单)
  (register-config-augmenter! api
    (lambda (cfg parsed)
      (let ((current-tools (cdr (assoc 'tools cfg))))
        (cons (cons 'tools (pi-blueprint-extend-tool-names current-tools parsed))
          (filter
            (lambda (p) (not (eq? (car p) 'tools)))
            cfg
          ) ;filter
        ) ;cons
      ) ;let
    ) ;lambda
  ) ;register-config-augmenter!

  ;; 5. 注册 flows
  (register-flow! api
    (make-extension-flow-def 'blueprint-local
      pi-blueprint-local-flow?
      (lambda (parsed cfg mode initial-msg)
        (run-blueprint-flow parsed cfg mode initial-msg)
      ) ;lambda
      :local?   #t
      :priority 20
    ) ;make-extension-flow-def
  ) ;register-flow!
  (register-flow! api
    (make-extension-flow-def 'blueprint-session
      pi-blueprint-flag?
      (lambda (parsed cfg mode initial-msg)
        (run-blueprint-flow parsed cfg mode initial-msg)
      ) ;lambda
      :local?   #f
      :priority 20
    ) ;make-extension-flow-def
  ) ;register-flow!

  ;; 6. 注册 prompt injector
  (register-prompt-injector! api
    (lambda (tool-calls)
      (let ((bp (pi-blueprint-current)))
        (if bp
          (let ((ctx (pi-build-blueprint-context bp)))
            (if (and (string? ctx) (not (string-null? ctx))) (string-trim-both ctx) #f)
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;lambda
  ) ;register-prompt-injector!

  "blueprint-loaded"
) ;define

blueprint-extension
