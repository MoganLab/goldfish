(import (liii check)
        (liii string)
        (liii pi red-green state)
        (liii pi red-green observe)
) ;import

(check-set-mode! 'report-failed)

(define (bash-args command)
  (list (cons "command" command))
) ;define

(define (phase)
  (pi-tdd-state-phase (pi-tdd-current-state))
) ;define

(define (last-run)
  (pi-tdd-state-last-test-run (pi-tdd-current-state))
) ;define

;; pi-tdd-observe-tool!
;; RED + 测试失败 → GREEN，并记录本次运行。
(pi-tdd-set-current-state! (pi-tdd-initial-state "task"))
(pi-tdd-observe-tool! "bash" (bash-args "pytest -q") "4 failed, 1 passed\n" #t)
(check (phase) => 'green)
(check (cdr (assoc 'passed (last-run))) => 1)
(check (cdr (assoc 'failed (last-run))) => 4)
(check (cdr (assoc 'turn-index (last-run))) => 0)
(check (pi-tdd-state-warning (pi-tdd-current-state)) => #f)

;; RED + 测试全通过 → 停留 RED 并给陈旧/既有行为告警文案。
(pi-tdd-set-current-state! (pi-tdd-initial-state "task"))
(pi-tdd-observe-tool! "bash" (bash-args "pytest -q") "3 passed\n" #f)
(check (phase) => 'red)
(check-true (string-contains? (pi-tdd-state-warning (pi-tdd-current-state))
              "testing existing behavior"
            ) ;string-contains?
) ;check-true

;; GREEN + 测试仍失败 → 停留 GREEN 并提示继续实现。
(pi-tdd-set-current-state!
  (let-values (((next warning) (pi-tdd-advance-phase (pi-tdd-initial-state "task") 'tests-fail))) ;pi-tdd-advance-phase
    next
  ) ;let-values
) ;pi-tdd-set-current-state!
(pi-tdd-observe-tool! "bash" (bash-args "pytest -q") "2 failed\n" #t)
(check (phase) => 'green)
(check-true (string-contains? (pi-tdd-state-warning (pi-tdd-current-state))
              "Continue implementing"
            ) ;string-contains?
) ;check-true

;; REFACTOR + 测试通过 → COMPLETE。
(pi-tdd-set-current-state!
  (let*-values (((a w1) (pi-tdd-advance-phase (pi-tdd-initial-state "task") 'tests-fail))
                ((b w2) (pi-tdd-advance-phase a 'tests-pass))
               ) ;
    b
  ) ;let-values
) ;pi-tdd-set-current-state!
(pi-tdd-observe-tool! "bash" (bash-args "pytest -q") "5 passed\n" #f)
(check (phase) => 'complete)

;; 非测试命令不触发检测。
(pi-tdd-set-current-state! (pi-tdd-initial-state "task"))
(pi-tdd-observe-tool! "bash" (bash-args "ls -la") "3 passed\n" #f)
(check (phase) => 'red)
(check (last-run) => #f)

;; 已停止/未激活时不记录。
(pi-tdd-set-current-state! #f)
(pi-tdd-observe-tool! "bash" (bash-args "pytest -q") "4 failed\n" #t)
(check (pi-tdd-current-state) => #f)

;; 写文件工具成功时记录分类；失败时不记录。
(pi-tdd-set-current-state! (pi-tdd-initial-state "task"))
(pi-tdd-observe-tool! "edit" (list (cons "path" "test_calc.py")) "" #f)
(check (pi-tdd-state-test-files (pi-tdd-current-state)) => '("test_calc.py"))
(pi-tdd-observe-tool! "edit" (list (cons "path" "calc.py")) "error" #t)
(check (pi-tdd-state-impl-files (pi-tdd-current-state)) => '())

(pi-tdd-set-current-state! #f)

(check-report)
