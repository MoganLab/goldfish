(import (liii check)
        (liii string)
        (liii pi tools)
        (liii pi tools record)
        (liii pi red-green state)
        (liii pi red-green tools)
) ;import

(check-set-mode! 'report-failed)

(define (result-text res)
  (let ((content (pi-tool-result-content res)))
    (cdr (assoc 'text (vector-ref content 0)))
  ) ;let
) ;define

;; 三个内置工具注册到工具表。
(check-true (pi-tool-definition? (get-tool-definition "tdd_status")))
(check-true (pi-tool-definition? (get-tool-definition "tdd_advance")))
(check-true (pi-tool-definition? (get-tool-definition "tdd_reset")))
(check (pi-tool-definition-name (get-tool-definition "tdd_advance")) => "tdd_advance")

;; pi-execute-tdd-status
;; 未激活时提示未激活；激活后返回阶段/任务/文件/测试结果。
(pi-tdd-set-current-state! #f)
(let ((res (pi-execute-tdd-status '())))
  (check (pi-tool-result-is-error res) => #f)
  (check (result-text res) => "TDD is not active.")
) ;let

(pi-tdd-set-current-state!
  (pi-tdd-add-test-file (pi-tdd-initial-state "实现登录") "test_login.py")
) ;pi-tdd-set-current-state!
(pi-tdd-set-current-state!
  (pi-tdd-set-last-test-run (pi-tdd-current-state)
    '((passed . 0) (failed . 2) (errors . 0) (exit-code . 1) (turn-index . 0))
  ) ;pi-tdd-set-last-test-run
) ;pi-tdd-set-current-state!
(let* ((text (result-text (pi-execute-tdd-status '()))))
  (check-true (string-contains? text "Phase: red"))
  (check-true (string-contains? text "Task: 实现登录"))
  (check-true (string-contains? text "Test files: test_login.py"))
  (check-true (string-contains? text "Impl files: (none)"))
  (check-true (string-contains? text "Last run: 0 passed, 2 failed, 0 errors"))
) ;let*

;; pi-execute-tdd-advance
;; idle 时报错；活动时推进阶段。
(pi-tdd-set-current-state! #f)
(let ((res (pi-execute-tdd-advance '())))
  (check (pi-tool-result-is-error res) => #t)
  (check-true (string-contains? (result-text res) "No active TDD session"))
) ;let

(pi-tdd-set-current-state! (pi-tdd-initial-state "任务"))
(let ((res (pi-execute-tdd-advance '((reason . "自动检测失灵")))))
  (check (pi-tool-result-is-error res) => #f)
  (check (result-text res) => "Advanced to GREEN phase.")
  (check (pi-tdd-state-phase (pi-tdd-current-state)) => 'green)
) ;let

;; pi-execute-tdd-reset
;; idle 时幂等；活动时回到 idle 并报告本次周期。
(pi-tdd-set-current-state! #f)
(let ((res (pi-execute-tdd-reset '())))
  (check (pi-tool-result-is-error res) => #f)
  (check (result-text res) => "TDD is already idle.")
) ;let

(let-values (((green warning) (pi-tdd-advance-phase (pi-tdd-initial-state "任务")
                                'tests-fail
                              ) ;pi-tdd-advance-phase
               ) ;
            ) ;
  (pi-tdd-set-current-state! green)
  (let ((res (pi-execute-tdd-reset '())))
    (check (pi-tool-result-is-error res) => #f)
    (check-true (string-contains? (result-text res) "TDD reset to idle"))
    (check-true (string-contains? (result-text res) "red -> green"))
    (check (pi-tdd-state-phase (pi-tdd-current-state)) => 'idle)
  ) ;let
) ;let-values

;; --exclude-tools 语义（工具过滤函数来自现有工具表）。
(check (exclude-tools-from-list '("read" "tdd_status" "tdd_advance")
         "tdd_status,tdd_advance"
       ) ;exclude-tools-from-list
  =>
  '("read")
) ;check

;; 通过 execute-tool-call 分发：错误标记不丢失。
(pi-tdd-set-current-state! #f)
(let ((res (execute-tool-call "tdd_status" '())))
  (check (pi-tool-result-is-error res) => #f)
  (check (result-text res) => "TDD is not active.")
) ;let
(let ((res (execute-tool-call "tdd_advance" '())))
  (check (pi-tool-result-is-error res) => #t)
  (check-true (string-contains? (result-text res) "No active TDD session"))
) ;let

(pi-tdd-set-current-state! #f)

(check-report)
