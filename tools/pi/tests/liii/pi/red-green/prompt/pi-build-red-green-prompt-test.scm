(import (liii check)
        (liii string)
        (liii pi red-green state)
        (liii pi red-green prompt)
) ;import

(check-set-mode! 'report-failed)

(define (green-state)
  (let-values (((next warning) (pi-tdd-advance-phase (pi-tdd-initial-state "登录") 'tests-fail))) ;pi-tdd-advance-phase
    next
  ) ;let-values
) ;define

(define (refactor-state)
  (let*-values (((green warning) (pi-tdd-advance-phase (pi-tdd-initial-state "登录") 'tests-fail)) ;pi-tdd-advance-phase
    
               ((next warning2) (pi-tdd-advance-phase green 'tests-pass)))
    next
  ) ;let-values
) ;define

;; pi-build-red-green-prompt
;; RED：任务、规则与验证门三要素。
(let* ((p (pi-build-red-green-prompt (pi-tdd-initial-state "实现用户登录"))))
  (check-true (string? p))
  (check-true (string-contains? p "## TDD: RED Phase -- Write Failing Tests"))
  (check-true (string-contains? p "**Task:** 实现用户登录"))
  (check-true (string-contains? p "Write failing tests"))
  (check-true (string-contains? p "After writing tests, RUN them"))
  (check-true (string-contains? p "FAIL because the feature is missing"))
  (check-true (string-contains? p "### Verification Gate"))
  (check-true (string-contains? p "Implementation files are write-protected"))
  (check-true (string-starts? p "\n\n"))
) ;let*

;; GREEN：最小实现 + 失败计数 + 跑测试确认通过。
(let* ((state (pi-tdd-set-last-test-run (green-state)
                '((passed . 0) (failed . 4) (errors . 0) (exit-code . 1) (turn-index . 0))
              ) ;pi-tdd-set-last-test-run
       ) ;
       (p (pi-build-red-green-prompt state))
      ) ;
  (check-true (string-contains? p "## TDD: GREEN Phase -- Make Tests Pass"))
  (check-true (string-contains? p "**Failing tests:** 4"))
  (check-true (string-contains? p "Write the minimal implementation"))
  (check-true (string-contains? p "RUN tests and confirm they pass"))
) ;let*

;; REFACTOR：保持绿、不加行为、可选阶段。
(let* ((p (pi-build-red-green-prompt (refactor-state))))
  (check-true (string-contains? p "## TDD: REFACTOR Phase -- Improve Without Breaking"))
  (check-true (string-contains? p "Keep tests green"))
  (check-true (string-contains? p "This phase is optional"))
) ;let*

;; COMPLETE / IDLE：不再注入阶段提示词。
(check (pi-build-red-green-prompt (pi-tdd-idle-state)) => #f)
(let-values (((complete warning) (pi-tdd-advance-phase (refactor-state) 'tests-pass)))
  (check (pi-build-red-green-prompt complete) => #f)
) ;let-values

;; 陈旧测试结果告警。
(let* ((state (pi-tdd-set-turn-index (green-state) 1))
       (state (pi-tdd-set-last-test-run state
                '((passed . 0) (failed . 4) (errors . 0) (exit-code . 1) (turn-index . 0))
              ) ;pi-tdd-set-last-test-run
       ) ;state
       (p (pi-build-red-green-prompt state))
      ) ;
  (check-true (string-contains? p "Stale test results"))
) ;let*

;; 顺序告警：RED 阶段先改实现文件。
(let* ((state (pi-tdd-add-impl-file (pi-tdd-initial-state "task") "calc.py"))
       (p (pi-build-red-green-prompt state))
      ) ;
  (check-true (string-contains? p "TDD Warning"))
  (check-true (string-contains? p "edited before any test files"))
) ;let*

;; 阶段推进告警被注入。
(let* ((state (pi-tdd-set-warning (pi-tdd-initial-state "task") "继续实现"))
       (p (pi-build-red-green-prompt state))
      ) ;
  (check-true (string-contains? p "**TDD warning:** 继续实现"))
) ;let*

;; pi-tdd-turn-system-prompt：按当前阶段每回合追加。
(pi-tdd-set-current-state! (pi-tdd-initial-state "任务"))
(let ((provider (pi-tdd-turn-system-prompt "BASE")))
  (check-true (string-starts? (provider '()) "BASE\n\n## TDD: RED Phase"))
  (let-values (((next warning) (pi-tdd-advance-phase (pi-tdd-current-state) 'tests-fail)))
    (pi-tdd-set-current-state! next)
  ) ;let-values
  (check-true (string-starts? (provider '()) "BASE\n\n## TDD: GREEN Phase"))
) ;let

;; 未激活时不改变基础提示词。
(pi-tdd-set-current-state! (pi-tdd-idle-state))
(check ((pi-tdd-turn-system-prompt "BASE") '()) => "BASE")
(pi-tdd-set-current-state! #f)
(check ((pi-tdd-turn-system-prompt "BASE") '()) => "BASE")

(check-report)
