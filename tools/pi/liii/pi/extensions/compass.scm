;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(import (scheme base)
  (liii string)
  (liii list)
  (liii path)
  (liii pi extension)
  (liii pi compass args)
  (liii pi compass types)
  (liii pi compass project)
  (liii pi compass store)
  (liii pi compass render)
  (liii pi compass tour)
  (liii pi compass tools)
  (liii pi compass prompt)
  (liii pi modes compass)
) ;import

(define (%ref parsed key)
  (let ((p (assoc key parsed)))
    (if p (cdr p) #f)
  ) ;let
) ;define

(define (compass-extension api . opt-context)
  ;; 1. 注册 flags
  (register-flag! api
    (make-extension-flag-def "--codemap" "Generate codemap" :key 'codemap)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--refresh" "Refresh codemap" :key 'refresh)
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--no-codemap-inject"
      "Disable injection"
      :key 'no-codemap-inject
    ) ;make-extension-flag-def
  ) ;register-flag!
  (register-flag! api
    (make-extension-flag-def "--codemap-file"
      "Codemap path"
      :key        'codemap-file
      :takes-arg? #t
    ) ;make-extension-flag-def
  ) ;register-flag!
  (register-flag! api
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

  ;; 2. 注册 tools
  (register-tool! api (pi-codebase-map-definition))
  (register-tool! api (pi-code-tour-definition))

  ;; 3. 注册 validator
  (register-validator! api (lambda (parsed . extra) (pi-compass-validate parsed)))

  ;; 4. 注册 config augmenter (扩展 tool 白名单)
  (register-config-augmenter! api
    (lambda (cfg parsed) (pi-compass-augment-config parsed cfg))
  ) ;register-config-augmenter!

  ;; 5. 注册 flow
  (register-flow! api
    (make-extension-flow-def 'compass-local
      pi-compass-local-flow?
      (lambda (parsed cfg mode initial-msg) (run-compass-flow parsed mode))
      :local?   #t
      :priority 30
    ) ;make-extension-flow-def
  ) ;register-flow!

  ;; 6. 注册 prompt injector
  (register-prompt-injector! api
    (lambda (tool-calls)
      (let* ((cwd (path->string (path-cwd))) (block (pi-compass-injection-text '() cwd)))
        (if block (string-trim-both block) #f)
      ) ;let*
    ) ;lambda
  ) ;register-prompt-injector!

  "compass-loaded"
) ;define

compass-extension
