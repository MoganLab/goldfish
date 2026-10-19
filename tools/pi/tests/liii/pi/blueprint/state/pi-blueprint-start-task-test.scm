(import (liii check)
        (liii pi blueprint types)
        (liii pi blueprint graph)
        (liii pi blueprint transition)
        (liii pi blueprint state)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "desc" '() '() deps)
) ;define

(define bp
  (pi-blueprint-create "bp-1" "objective" "proj"
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

(define (task-status b id)
  (pi-task-status (pi-blueprint-find-task (pi-blueprint-all-tasks b) id))
) ;define

;; pi-blueprint-start-task
(let* ((bp1 (pi-blueprint-start-task bp "1.1" "session-1"))
       (t (pi-blueprint-find-task (pi-blueprint-all-tasks bp1) "1.1"))
       (p (pi-blueprint-find-phase bp1 "1"))
      ) ;
  (check (pi-task-status t) => "in_progress")
  (check (pi-blueprint-jref t "session_id" #f) => "session-1")
  (check-true (string? (pi-blueprint-jref t "started_at" #f)))
  (check (pi-phase-status p) => "active")
  (check (pi-blueprint-active-task-id bp1) => "1.1")
  (check (pi-blueprint-active-phase-id bp1) => "1")
) ;let*

;; 未知任务不改变状态。
(check (pi-blueprint-start-task bp "9.9" #f) => bp)

;; 阶段状态由 pending 变 active 后保持。
(let* ((bp1 (pi-blueprint-start-task bp "1.1" #f))
       (bp2 (pi-blueprint-start-task bp1 "1.1" #f))
      ) ;
  (check (pi-phase-status (pi-blueprint-find-phase bp2 "1")) => "active")
) ;let*

(check-report)
