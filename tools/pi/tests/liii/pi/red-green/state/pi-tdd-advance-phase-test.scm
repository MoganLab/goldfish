(import (liii check)
        (liii string)
        (liii pi red-green state)
) ;import

(check-set-mode! 'report-failed)

(define (advance state trigger)
  (let-values (((next warning) (pi-tdd-advance-phase state trigger)))
    (list next warning)
  ) ;let-values
) ;define

(define (result-state pair)
  (car pair)
) ;define

(define (result-warning pair)
  (cadr pair)
) ;define

;; pi-tdd-advance-phase
;; 阶段转移表与 native pi-red-green 的 state-machine 语义一致。

;; 初始状态：RED，历史只有一段。
(let ((s (pi-tdd-initial-state "实现登录")))
  (check (pi-tdd-state-phase s) => 'red)
  (check (pi-tdd-state-task s) => "实现登录")
  (check (length (pi-tdd-state-phase-history s)) => 1)
  (check (cdr (assoc 'phase (car (pi-tdd-state-phase-history s)))) => 'red)
  (check (pi-tdd-state-turn-index s) => 0)
)

;; RED + 测试失败 → GREEN，无告警，历史追加一段。
(let* ((s (pi-tdd-initial-state "task"))
       (r (advance s 'tests-fail))
      ) ;
  (check (pi-tdd-state-phase (result-state r)) => 'green)
  (check (result-warning r) => #f)
  (check (length (pi-tdd-state-phase-history (result-state r))) => 2)
  (let ((entry (car (reverse (pi-tdd-state-phase-history (result-state r))))))
    (check (cdr (assoc 'phase entry)) => 'green)
    (check-true (string? (cdr (assoc 'entered-at entry))))
  ) ;let
) ;let*

;; RED + 测试报错 → 停留 RED，提示先修测试错误。
(let* ((r (advance (pi-tdd-initial-state "task") 'tests-error)))
  (check (pi-tdd-state-phase (result-state r)) => 'red)
  (check-true (string-contains? (result-warning r) "Fix the test error"))
) ;let*

;; RED + 全通过 → 停留 RED，提示测试的是既有行为。
(let* ((r (advance (pi-tdd-initial-state "task") 'tests-pass)))
  (check (pi-tdd-state-phase (result-state r)) => 'red)
  (check-true (string-contains? (result-warning r) "testing existing behavior"))
) ;let*

;; GREEN + 全通过 → REFACTOR。
(let* ((green (result-state (advance (pi-tdd-initial-state "task") 'tests-fail)))
       (r (advance green 'tests-pass))
      ) ;
  (check (pi-tdd-state-phase (result-state r)) => 'refactor)
  (check (result-warning r) => #f)
) ;let*

;; GREEN + 仍失败 → 停留 GREEN，提示继续实现。
(let* ((green (result-state (advance (pi-tdd-initial-state "task") 'tests-fail)))
       (r (advance green 'tests-fail))
      ) ;
  (check (pi-tdd-state-phase (result-state r)) => 'green)
  (check-true (string-contains? (result-warning r) "Continue implementing"))
) ;let*

;; REFACTOR + 全通过 → COMPLETE。
(let* ((green (result-state (advance (pi-tdd-initial-state "task") 'tests-fail)))
       (refactor (result-state (advance green 'tests-pass)))
       (r (advance refactor 'tests-pass))
      ) ;
  (check (pi-tdd-state-phase (result-state r)) => 'complete)
  (check (length (pi-tdd-state-phase-history (result-state r))) => 4)
) ;let*

;; REFACTOR + 失败 → 停留 REFACTOR，提示回滚。
(let* ((green (result-state (advance (pi-tdd-initial-state "task") 'tests-fail)))
       (refactor (result-state (advance green 'tests-pass)))
       (r (advance refactor 'tests-fail))
      ) ;
  (check (pi-tdd-state-phase (result-state r)) => 'refactor)
  (check-true (string-contains? (result-warning r) "Revert the last change"))
) ;let*

;; 手动推进走同一转移表；COMPLETE 后手动推进只给提示不改状态。
(let* ((r1 (advance (pi-tdd-initial-state "task") 'manual-advance))
       (r2 (advance (result-state r1) 'manual-advance))
       (r3 (advance (result-state r2) 'manual-advance))
       (r4 (advance (result-state r3) 'manual-advance))
      ) ;
  (check (pi-tdd-state-phase (result-state r1)) => 'green)
  (check (pi-tdd-state-phase (result-state r2)) => 'refactor)
  (check (pi-tdd-state-phase (result-state r3)) => 'complete)
  (check (pi-tdd-state-phase (result-state r4)) => 'complete)
  (check-true (string-contains? (result-warning r4) "already complete"))
) ;let*

;; reset 从 RED 回到 IDLE。
(let* ((r (advance (pi-tdd-initial-state "task") 'reset)))
  (check (pi-tdd-state-phase (result-state r)) => 'idle)
  (check (pi-tdd-state-task (result-state r)) => "")
) ;let*

;; IDLE 上的非 reset 转移给告警但不改变状态。
(let* ((r (advance (pi-tdd-idle-state) 'tests-pass)))
  (check (pi-tdd-state-phase (result-state r)) => 'idle)
  (check-true (string-contains? (result-warning r) "No active TDD session"))
) ;let*

(check-report)
