(import (liii check)
        (liii string)
        (liii pi blueprint types)
        (liii pi blueprint graph)
        (liii pi blueprint transition)
        (liii pi blueprint state)
) ;import

(check-set-mode! 'report-failed)

(define gate (pi-blueprint-gate "custom_command" "Run tests" "true"))
(define bp
  (pi-blueprint-create "bp-1" "objective" "proj"
    (list (pi-blueprint-phase "1" "Phase 1" ""
            (list (pi-blueprint-task "1.1" "T1" "" '() '() '())
              (pi-blueprint-task "1.2" "T2" "" '() '() '("1.1"))
            ) ;list
            (list gate)
          ) ;pi-blueprint-phase
    ) ;list
  ) ;pi-blueprint-create
) ;define

;; pi-blueprint-set-gate
;; 通过时记录 passed 与 last_checked_at，清空错误信息。
(let* ((bp1 (pi-blueprint-set-gate bp "1" 0 #t #f))
       (g (car (pi-phase-gates (pi-blueprint-find-phase bp1 "1"))))
      ) ;
  (check (pi-gate-passed? g) => #t)
  (check-true (string? (pi-gate-last-checked g)))
  (check (pi-gate-error-message g) => #f)
) ;let*

;; 失败时记录错误摘要。
(let* ((bp1 (pi-blueprint-set-gate bp "1" 0 #f "boom"))
       (g (car (pi-phase-gates (pi-blueprint-find-phase bp1 "1"))))
      ) ;
  (check (pi-gate-passed? g) => #f)
  (check (pi-gate-error-message g) => "boom")
) ;let*

;; 未知阶段/索引安全返回原状态。
(check (pi-blueprint-set-gate bp "9" 0 #t #f) => bp)
(check (pi-blueprint-set-gate bp "1" 9 #t #f) => bp)

(check-report)
