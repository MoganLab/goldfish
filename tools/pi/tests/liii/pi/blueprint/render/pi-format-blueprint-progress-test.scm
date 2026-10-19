(import (liii check)
        (liii pi blueprint types)
        (liii pi blueprint transition)
        (liii pi blueprint render)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "desc" '() '() deps)
) ;define

(define bp
  (pi-blueprint-create "bp-1" "obj" "proj"
    (list (pi-blueprint-phase "1" "Phase 1" ""
            (list (task "1.1" '()) (task "1.2" '("1.1")))
            '()
          ) ;pi-blueprint-phase
      (pi-blueprint-phase "2" "Phase 2" ""
        (list (task "2.1" '("1.2")) (task "2.2" '("2.1")))
        '()
      ) ;pi-blueprint-phase
    ) ;list
  ) ;pi-blueprint-create
) ;define

;; pi-format-blueprint-progress
(check (pi-format-blueprint-progress bp) => "Progress: 0/4 tasks (0%)")
(check (pi-format-blueprint-progress (pi-blueprint-complete-task bp "1.1"))
  =>
  "Progress: 1/4 tasks (25%)"
) ;check
(check (pi-blueprint-task-counts bp) => '((done . 0) (total . 4)))

;; 空任务集合不除零。
(check (pi-format-blueprint-progress (pi-blueprint-create "x" "o" "p" '()))
  =>
  "Progress: 0/0 tasks (0%)"
) ;check

(check-report)
