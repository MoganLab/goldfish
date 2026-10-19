(import (liii check)
        (liii string)
        (liii pi red-green state)
        (liii pi red-green report)
) ;import

(check-set-mode! 'report-failed)

;; pi-format-tdd-summary
;; 输出当前阶段、任务、文件列表、最后一次测试结果与阶段历史。

;; 初始 RED 状态：无文件、无测试运行。
(let* ((text (pi-format-tdd-summary (pi-tdd-initial-state "实现登录"))))
  (check-true (string-contains? text "## TDD Summary"))
  (check-true (string-contains? text "Phase: red"))
  (check-true (string-contains? text "Task: 实现登录"))
  (check-true (string-contains? text "Test files: (none)"))
  (check-true (string-contains? text "Implementation files: (none)"))
  (check-true (string-contains? text "Last test run: (none)"))
  (check-true (string-contains? text "Phase history: red"))
  (check-true (string-ends? text "\n"))
) ;let*

;; 推进后：阶段历史、文件列表与测试结果。
(let* ((state (pi-tdd-initial-state "实现登录"))
       (state (pi-tdd-add-test-file state "test_calc.py"))
       (state (pi-tdd-set-last-test-run state
                '((passed . 0) (failed . 4) (errors . 0) (exit-code . 1) (turn-index . 0))
              ) ;pi-tdd-set-last-test-run
       ) ;state
       (text (pi-format-tdd-summary state))
      ) ;
  (check-true (string-contains? text "Test files: test_calc.py"))
  (check-true (string-contains? text "Last test run: 0 passed, 4 failed, 0 errors"))
) ;let*

(let*-values (((green warning) (pi-tdd-advance-phase (pi-tdd-initial-state "任务") 'tests-fail))
              ((refactor warning2) (pi-tdd-advance-phase green 'tests-pass))
             ((complete warning3) (pi-tdd-advance-phase refactor 'tests-pass))
            ) ;
  (let* ((state (pi-tdd-add-test-file complete "test_calc.py"))
         (state (pi-tdd-add-impl-file state "calc.py"))
         (text (pi-format-tdd-summary state))
        ) ;
    (check-true (string-contains? text "Phase: complete"))
    (check-true (string-contains? text "Test files: test_calc.py"))
    (check-true (string-contains? text "Implementation files: calc.py"))
    (check-true (string-contains? text "Phase history: red -> green -> refactor -> complete"))
  ) ;let*
) ;let-values

;; 告警被展示。
(let* ((state (pi-tdd-set-warning (pi-tdd-initial-state "任务") "先写测试"))
       (text (pi-format-tdd-summary state))
      ) ;
  (check-true (string-contains? text "Warning: 先写测试"))
) ;let*

;; IDLE 摘要。
(let ((text (pi-format-tdd-summary (pi-tdd-idle-state))))
  (check-true (string-contains? text "Phase: idle"))
  (check-true (string-contains? text "Task: (none)"))
) ;let

(check-report)
