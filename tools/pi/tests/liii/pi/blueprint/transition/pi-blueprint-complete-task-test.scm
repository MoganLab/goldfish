(import (liii check)
        (liii list)
        (liii pi blueprint types)
        (liii pi blueprint graph)
        (liii pi blueprint transition)
        (liii pi blueprint state)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "desc" '() '() deps)
) ;define

(define (phase id tasks gates)
  (pi-blueprint-phase id (string-append "Phase " id) "" tasks gates)
) ;define

(define (make-bp)
  (pi-blueprint-create "bp-1" "objective" "proj"
    (list (phase "1" (list (task "1.1" '()) (task "1.2" '("1.1"))) '())
      (phase "2" (list (task "2.1" '("1.2")) (task "2.2" '("2.1"))) '())
    ) ;list
  ) ;pi-blueprint-create
) ;define

(define (status-of bp id)
  (pi-task-status (pi-blueprint-find-task (pi-blueprint-all-tasks bp) id))
) ;define

;; pi-blueprint-complete-task
;; 完成后解阻下一任务，阶段全部完成时阶段状态置 completed。
(let* ((bp (pi-blueprint-complete-task (make-bp) "1.1")))
  (check (status-of bp "1.1") => "completed")
  (check (status-of bp "1.2") => "pending")
  (check (pi-blueprint-active-task-id bp) => "1.2")
  (check (pi-phase-status (pi-blueprint-find-phase bp "1")) => "pending")
) ;let*

;; 阶段内任务全完成 → 阶段 completed，活动任务前移到下一阶段。
(let* ((bp
         (pi-blueprint-complete-task
           (pi-blueprint-complete-task (make-bp) "1.1")
           "1.2"
         ) ;pi-blueprint-complete-task
       ) ;
      ) ;
  (check (pi-phase-status (pi-blueprint-find-phase bp "1")) => "completed")
  (check (pi-blueprint-active-task-id bp) => "2.1")
) ;let*

;; 全部任务完成且门禁全过 → blueprint completed。
(let* ((bp
         (let loop
           ((b (make-bp)) (ids '("1.1" "1.2" "2.1" "2.2")))
           (if (null? ids) b (loop (pi-blueprint-complete-task b (car ids)) (cdr ids)))
         ) ;let
       ) ;
      ) ;
  (check (pi-blueprint-status bp) => "completed")
  (check (pi-blueprint-active-task-id bp) => #f)
) ;let*

;; 已完成任务重复完成是幂等的。
(let* ((bp (pi-blueprint-complete-task (make-bp) "1.1"))
       (bp2 (pi-blueprint-complete-task bp "1.1"))
      ) ;
  (check (status-of bp2 "1.1") => "completed")
) ;let*

;; skip 与 complete 一样视为完成。
(let* ((bp (pi-blueprint-skip-task (make-bp) "1.1")))
  (check (status-of bp "1.1") => "skipped")
  (check (status-of bp "1.2") => "pending")
  (check-true (string? (pi-blueprint-jref
                         (pi-blueprint-find-task (pi-blueprint-all-tasks bp) "1.1")
                         "completed_at"
                         #f
                       ) ;pi-blueprint-jref
              ) ;string?
  ) ;check-true
) ;let*

(check-report)
