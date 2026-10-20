;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension builtin prompts)
  (import (scheme base)
    (liii string)
    (liii path)
    (liii pi extension registry)
    (liii pi blueprint runtime)
    (liii pi blueprint prompt)
    (liii pi compass prompt)
  ) ;import
  (export register-builtin-prompt-injectors!)
  (begin

    (define (register-builtin-prompt-injectors! reg)
      ;; 1. blueprint: 如果有正在运行的蓝图则注入上下文与任务约束
      (register-prompt-injector! reg
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

      ;; 2. compass: 如果工程存在 codemap 缓存则注入拓扑摘要
      (register-prompt-injector! reg
        (lambda (tool-calls)
          (let* ((cwd (path->string (path-cwd))) (block (pi-compass-injection-text '() cwd)))
            (if block (string-trim-both block) #f)
          ) ;let*
        ) ;lambda
      ) ;register-prompt-injector!

      reg
    ) ;define

  ) ;begin
) ;define-library
