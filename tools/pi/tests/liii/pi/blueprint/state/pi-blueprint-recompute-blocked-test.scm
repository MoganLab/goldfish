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

(define (phase id tasks)
  (pi-blueprint-phase id (string-append "Phase " id) "" tasks '())
) ;define

(define (make-bp)
  (pi-blueprint-create "bp-1" "objective" "proj"
    (list (phase "1" (list (task "1.1" '()) (task "1.2" '("1.1"))))
      (phase "2" (list (task "2.1" '("1.2")) (task "2.2" '("2.1"))))
    ) ;list
  ) ;pi-blueprint-create
) ;define

(define (task-status bp id)
  (pi-task-status (pi-blueprint-find-task (pi-blueprint-all-tasks bp) id))
) ;define

;; pi-blueprint-recompute-blocked
;; 创建即重算：无依赖 pending，依赖未满足 blocked（跨阶段同样生效）。
(let ((bp (make-bp)))
  (check (task-status bp "1.1") => "pending")
  (check (task-status bp "1.2") => "blocked")
  (check (task-status bp "2.1") => "blocked")
  (check (task-status bp "2.2") => "blocked")
  (check (pi-blueprint-active-task-id bp) => "1.1")
) ;let

;; 完成 1.1 后自动解阻 1.2，跨阶段依赖仍阻塞。
(let* ((bp (pi-blueprint-complete-task (make-bp) "1.1")))
  (check (task-status bp "1.2") => "pending")
  (check (task-status bp "2.1") => "blocked")
  (check (pi-blueprint-active-task-id bp) => "1.2")
) ;let*

;; 幂等：重复重算不改变状态。
(let* ((bp (make-bp)) (bp1 (pi-blueprint-recompute-blocked bp))
       (bp2 (pi-blueprint-recompute-blocked bp1))
      ) ;
  (check (map pi-task-status (pi-blueprint-all-tasks bp2))
    =>
    (map pi-task-status (pi-blueprint-all-tasks bp1))
  ) ;check
) ;let*

(check-report)
