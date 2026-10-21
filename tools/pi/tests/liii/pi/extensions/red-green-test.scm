(import (liii check)
  (liii path)
  (liii pi extension)
  (liii pi red-green state)
) ;import

(check-set-mode! 'report-failed)

(define (get-red-green-path)
  (cond
    ((file-exists? "tools/pi/liii/pi/extensions/red-green.scm")
     "tools/pi/liii/pi/extensions/red-green.scm")
    ((file-exists? "liii/pi/extensions/red-green.scm")
     "liii/pi/extensions/red-green.scm")
    (else "../../liii/pi/extensions/red-green.scm")
  ) ;cond
) ;define

;; 1. 加载 red-green.scm 扩展
(let* ((api (make-extension-api))
       (res (pi-load-extension (get-red-green-path) api)))
  (check (pair? res) => #t)
  (check (eq? (cdr (assq 'status res)) 'ok) => #t)

  ;; 2. 验证 flags
  (let ((flag (find-extension-flag api "--red-green")))
    (check (extension-flag-def? flag) => #t)
    (check (eq? (extension-flag-def-key flag) 'red-green) => #t)
  ) ;let

  ;; 3. 验证 commands: /tdd 与 /tdd-status
  (let ((cmd-tdd (find-extension-command api "/tdd"))
        (cmd-tdd-status (find-extension-command api "/tdd-status")))
    (check (extension-command-def? cmd-tdd) => #t)
    (check (extension-command-def? cmd-tdd-status) => #t)
    (check (extension-command-def? (find-extension-command api "tdd")) => #t)
    (check (extension-command-def? (find-extension-command api "tdd-status")) => #t)

    ;; 测试 command 执行逻辑
    ;; 3.1 初始未激活状态查看
    (let ((msg ((extension-command-def-handler cmd-tdd) "" #f)))
      (check (string? msg) => #t)
    ) ;let

    ;; 3.2 激活 TDD：/tdd <task>
    (let ((msg ((extension-command-def-handler cmd-tdd) "实现加法功能" #f)))
      (check (string? msg) => #t)
      (check (eq? (pi-tdd-state-phase (pi-tdd-current-state)) 'red) => #t)
    ) ;let

    ;; 3.3 激活后状态查看：/tdd-status
    (let ((status-msg ((extension-command-def-handler cmd-tdd-status) "" #f)))
      (check (string? status-msg) => #t)
      (check (string-contains? status-msg "Phase: red") => #t)
    ) ;let

    ;; 3.4 停用 TDD：/tdd off
    (let ((off-msg ((extension-command-def-handler cmd-tdd) "off" #f)))
      (check (string? off-msg) => #t)
      (check (eq? (pi-tdd-state-phase (pi-tdd-current-state)) 'idle) => #t)
    ) ;let
  ) ;let

  ;; 4. 验证 tools: tdd_status, tdd_advance, tdd_reset
  (check (extension-tool-def? (find-extension-tool api "tdd_status")) => #t)
  (check (extension-tool-def? (find-extension-tool api "tdd_advance")) => #t)
  (check (extension-tool-def? (find-extension-tool api "tdd_reset")) => #t)

  ;; 5. 验证 flows
  (let ((flow (find-extension-flow api 'red-green)))
    (check (extension-flow-def? flow) => #t)
    (check (eq? (extension-flow-def-name flow) 'red-green) => #t)
  ) ;let

  ;; 6. 验证 prompt-injectors
  (check (pair? (extension-registry-prompt-injectors-list api)) => #t)

  ;; 7. 验证 lifecycle hooks: tool_call, tool_result, turn_start
  (check (pair? (extension-registry-get-listeners api 'tool_call)) => #t)
  (check (pair? (extension-registry-get-listeners api 'tool_result)) => #t)
  (check (pair? (extension-registry-get-listeners api 'turn_start)) => #t)

  ;; 8. 验证 validators
  (let ((validators (extension-registry-validators-list api)))
    (check (pair? validators) => #t)
    (let ((v (car validators)))
      ;; 正常合法参数通过
      (check (v '((red-green . #t)) "实现功能" '()) => (if #f #f))
      ;; --red-green 缺少任务描述
      (check-catch 'pi-arg-error
        (v '((red-green . #t)) "" '()))
      ;; --red-green 与 --code-review 冲突
      (check-catch 'pi-arg-error
        (v '((red-green . #t) (code-review . #t)) "x" '()))
      ;; --red-green 与 --apply 冲突
      (check-catch 'pi-arg-error
        (v '((red-green . #t) (apply . #t)) "x" '()))
    ) ;let
  ) ;let
) ;let*

(check-report)
