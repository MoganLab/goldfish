(import (liii check)
        (liii pi blueprint types)
        (liii pi blueprint graph)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "desc" '() '() deps)
) ;define

(define tasks
  (list (task "1.1" '())
    (task "1.2" '("1.1"))
    (task "1.3" '("1.1" "1.2"))
    (pi-blueprint-assoc-set (task "1.4" '()) "status" "completed")
  ) ;list
) ;define

;; pi-blueprint-ready?
(check (pi-blueprint-ready? tasks "1.1") => #t)
(check (pi-blueprint-ready? tasks "1.2") => #f)
(check (pi-blueprint-ready? tasks "1.3") => #f)
(check (pi-blueprint-ready? tasks "1.4") => #f)
(check (pi-blueprint-ready? tasks "9.9") => #f)

;; pi-blueprint-blocked-ids：未完成且依赖未满足的任务。
(check (pi-blueprint-blocked-id-list tasks) => '("1.2" "1.3"))
(check (pi-blueprint-blocking-tasks tasks "1.3") => '("1.1" "1.2"))
(check (pi-blueprint-blocking-tasks tasks "1.1") => '())

(check-report)
