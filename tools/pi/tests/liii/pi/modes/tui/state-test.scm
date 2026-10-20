(import (liii check)
  (liii error)
  (liii pi modes tui state)
) ;import

(check-set-mode! 'report-failed)

;; 1. make-pi-tui-state 默认构造
(let ((st (make-pi-tui-state)))
  (check-true (pi-tui-state? st))
  (check (state-status st) => 'idle)
  (check (state-messages st) => '())
  (check (state-input-editor st) => "")
  (check (state-active-tool st) => #f)
  (check (state-scroll-offset st) => 0)
  (check (state-dirty? st) => #t)
  ;; 别名访问器测试
  (check (pi-tui-state-status st) => 'idle)
  (check (pi-tui-state-messages st) => '())
  (check (pi-tui-state-input-editor st) => "")
  (check (pi-tui-state-active-tool st) => #f)
  (check (pi-tui-state-scroll-offset st) => 0)
  (check (pi-tui-state-dirty? st) => #t)
) ;let

;; 2. make-pi-tui-state 自定义参数构造
(let ((st (make-pi-tui-state :status 'running
            :messages '((user . "hello"))
            :input-editor "draft"
            :active-tool 'bash
            :scroll-offset 3
            :dirty? #f
          ) ;make-pi-tui-state
      ) ;st
     ) ;
  (check-true (pi-tui-state? st))
  (check (state-status st) => 'running)
  (check (state-messages st) => '((user . "hello")))
  (check (state-input-editor st) => "draft")
  (check (state-active-tool st) => 'bash)
  (check (state-scroll-offset st) => 3)
  (check (state-dirty? st) => #f)
) ;let

;; 3. state-valid-status? 谓词验证
(check-true (state-valid-status? 'idle))
(check-true (state-valid-status? 'running))
(check-true (state-valid-status? 'streaming))
(check-true (state-valid-status? 'confirm))
(check-true (state-valid-status? 'exit))
(check-false (state-valid-status? 'unknown))
(check-false (state-valid-status? "idle"))
(check-false (state-valid-status? 123))
(check-false (state-valid-status? #f))

;; 4. 构造参数校验（异常路径）
(check-catch 'type-error (make-pi-tui-state :status 'invalid-status))
(check-catch 'type-error (make-pi-tui-state :messages "not-a-list"))
(check-catch 'type-error (make-pi-tui-state :scroll-offset -1))
(check-catch 'type-error (make-pi-tui-state :scroll-offset "zero"))
(check-catch 'type-error (make-pi-tui-state :dirty? "true"))

;; 5. 函数式纯更新原语（state-with-*）
(let* ((s0 (make-pi-tui-state :dirty? #f))
       (s1 (state-with-status s0 'running))
       (s2 (state-with-messages s1 '((assistant . "ok"))))
       (s3 (state-with-input-editor s2 "input"))
       (s4 (state-with-active-tool s3 "write"))
       (s5 (state-with-scroll-offset s4 10))
       (s6 (state-with-dirty s5 #f))
      ) ;
  ;; 原对象不可变保护
  (check (state-status s0) => 'idle)
  (check (state-dirty? s0) => #f)
  ;; 各级纯更新结果与脏标记自动激活
  (check (state-status s1) => 'running)
  (check (state-dirty? s1) => #t)
  (check (state-messages s2) => '((assistant . "ok")))
  (check (state-dirty? s2) => #t)
  (check (state-input-editor s3) => "input")
  (check (state-dirty? s3) => #t)
  (check (state-active-tool s4) => "write")
  (check (state-dirty? s4) => #t)
  (check (state-scroll-offset s5) => 10)
  (check (state-dirty? s5) => #t)
  (check (state-dirty? s6) => #f)
) ;let*

;; 6. state-with-* 非法参数异常检查
(let ((st (make-pi-tui-state)))
  (check-catch 'type-error (state-with-status "not-state" 'running))
  (check-catch 'type-error (state-with-status st 'invalid))
  (check-catch 'type-error (state-with-messages st "not-list"))
  (check-catch 'type-error (state-with-scroll-offset st -5))
  (check-catch 'type-error (state-with-dirty st "bad-bool"))
) ;let

;; 7. 受控原地变异原语（state-set-*! 与 dirty 标志控制）
(let ((st (make-pi-tui-state)))
  (check (state-dirty? st) => #t)
  (state-clear-dirty! st)
  (check (state-dirty? st) => #f)
  (state-mark-dirty! st)
  (check (state-dirty? st) => #t)

  (state-clear-dirty! st)
  (state-set-status! st 'streaming)
  (check (state-status st) => 'streaming)
  (check (state-dirty? st) => #t)

  (state-clear-dirty! st)
  (state-set-messages! st '((user . "prompt")))
  (check (state-messages st) => '((user . "prompt")))
  (check (state-dirty? st) => #t)

  (state-clear-dirty! st)
  (state-set-input-editor! st "new text")
  (check (state-input-editor st) => "new text")
  (check (state-dirty? st) => #t)

  (state-clear-dirty! st)
  (state-set-active-tool! st 'read)
  (check (state-active-tool st) => 'read)
  (check (state-dirty? st) => #t)

  (state-clear-dirty! st)
  (state-set-scroll-offset! st 42)
  (check (state-scroll-offset st) => 42)
  (check (state-dirty? st) => #t)
) ;let

;; 8. 受控原地变异非法参数异常检查
(let ((st (make-pi-tui-state)))
  (check-catch 'type-error (state-set-status! "not-state" 'idle))
  (check-catch 'type-error (state-set-status! st 'invalid))
  (check-catch 'type-error (state-set-messages! st "not-list"))
  (check-catch 'type-error (state-set-scroll-offset! st -1))
  (check-catch 'type-error (state-mark-dirty! "not-state"))
  (check-catch 'type-error (state-clear-dirty! "not-state"))
) ;let

(check-report)
