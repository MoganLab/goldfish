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

(define (gate desc)
  (pi-blueprint-gate "custom_command" desc "true")
) ;define

(define (phase id tasks gates)
  (pi-blueprint-phase id (string-append "Phase " id) "" tasks gates)
) ;define

(define (make-bp)
  (pi-blueprint-create "bp-1" "objective" "proj"
    (list (phase "1" (list (task "1.1" '()) (task "1.2" '("1.1")))
            (list (gate "gate one"))
          ) ;phase
      (phase "2" (list (task "2.1" '("1.2")) (task "2.2" '("2.1")))
        (list (gate "gate two"))
      ) ;phase
    ) ;list
  ) ;pi-blueprint-create
) ;define

(define (done-tasks bp)
  (let loop
    ((b bp) (ids '("1.1" "1.2")))
    (if (null? ids) b (loop (pi-blueprint-complete-task b (car ids)) (cdr ids)))
  ) ;let
) ;define

;; pi-blueprint-advance-phase
;; 任务全完成但门禁未过 → 不推进。
(let ((bp (done-tasks (make-bp))))
  (check (pi-blueprint-advance-phase bp) => bp)
) ;let

;; 门禁通过 → 阶段 verified，活动阶段与任务前移。
(let* ((bp1 (pi-blueprint-set-gate (done-tasks (make-bp)) "1" 0 #t #f))
       (bp2 (pi-blueprint-advance-phase bp1))
      ) ;
  (check (pi-phase-status (pi-blueprint-find-phase bp2 "1")) => "verified")
  (check (pi-blueprint-active-phase-id bp2) => "2")
  (check (pi-blueprint-active-task-id bp2) => "2.1")
  (check (pi-phase-status (pi-blueprint-find-phase bp2 "2")) => "pending")
) ;let*

;; 最后一个阶段 verified → blueprint completed，活动指针清空。
(let* ((bp1 (done-tasks (make-bp)))
       (bp2 (pi-blueprint-set-gate bp1 "1" 0 #t #f))
       (bp3 (pi-blueprint-advance-phase bp2))
       (bp4 (pi-blueprint-complete-task (pi-blueprint-complete-task bp3 "2.1") "2.2"))
       (bp5 (pi-blueprint-set-gate bp4 "2" 0 #t #f))
       (bp6 (pi-blueprint-advance-phase bp5))
      ) ;
  (check (pi-blueprint-status bp6) => "completed")
  (check (pi-blueprint-active-phase-id bp6) => #f)
  (check (pi-blueprint-active-task-id bp6) => #f)
) ;let*

(check-report)
