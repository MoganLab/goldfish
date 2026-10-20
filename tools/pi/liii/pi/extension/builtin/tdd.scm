;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension builtin tdd)
  (import (scheme base)
    (liii pi extension event)
    (liii pi extension registry)
    (liii pi red-green state)
    (liii pi red-green gate)
    (liii pi red-green observe)
  ) ;import
  (export register-builtin-tdd-hooks!)
  (begin

    (define (register-builtin-tdd-hooks! reg)
      ;; 1. tool_call 拦截器：TDD 写保护
      (pi-on! reg
        'tool_call
        (lambda (payload)
          (if (pi-tdd-current-state)
            (let* ((name (cdr (or (assq 'name payload) '(#f . ""))))
                   (args (cdr (or (assq 'input payload) '(#f #_quote ()))))
                   (gate-msg (pi-tdd-write-gate name args))
                  ) ;
              (if gate-msg (list 'block gate-msg) 'continue)
            ) ;let*
            'continue
          ) ;if
        ) ;lambda
      ) ;pi-on!

      ;; 2. tool_result 监听器：TDD 测试观察与阶段推进
      (pi-on! reg
        'tool_result
        (lambda (payload)
          (when (pi-tdd-current-state)
            (let ((name (cdr (or (assq 'name payload) '(#f . ""))))
                  (args (cdr (or (assq 'input payload) '(#f #_quote ()))))
                  (text (cdr (or (assq 'result payload) '(#f . ""))))
                  (is-err? (cdr (or (assq 'isError payload) '(#f . #f))))
                 ) ;
              (pi-tdd-observe-tool! name args text is-err?)
            ) ;let
          ) ;when
        ) ;lambda
      ) ;pi-on!

      ;; 3. turn_start 监听器：TDD 轮次自增
      (pi-on! reg
        'turn_start
        (lambda (payload) (when (pi-tdd-current-state) (pi-tdd-bump-turn!)))
      ) ;pi-on!

      reg
    ) ;define

    (add-registry-initializer! register-builtin-tdd-hooks!)

  ) ;begin
) ;define-library
