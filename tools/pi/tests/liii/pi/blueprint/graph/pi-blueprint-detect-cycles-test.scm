(import (liii check)
        (liii pi blueprint types)
        (liii pi blueprint graph)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "desc" '("ok") '("a.scm") deps)
) ;define

;; pi-blueprint-detect-cycles
;; 无环图返回空列表。
(check (pi-blueprint-detect-cycles
         (list (task "1.1" '()) (task "1.2" '("1.1")))
       ) ;pi-blueprint-detect-cycles
  =>
  '()
) ;check

;; 两节点环：依赖指向彼此。
(let* ((tasks (list (task "1.1" '("1.2")) (task "1.2" '("1.1"))))
       (cycles (pi-blueprint-detect-cycles tasks))
      ) ;
  (check (length cycles) => 1)
  (check (and (member "1.1" (car cycles)) #t) => #t)
  (check (and (member "1.2" (car cycles)) #t) => #t)
) ;let*

;; 三节点环。
(let* ((tasks (list (task "1.1" '("1.3"))
               (task "1.2" '("1.1"))
               (task "1.3" '("1.2"))
             ) ;list
       ) ;tasks
       (cycles (pi-blueprint-detect-cycles tasks))
      ) ;
  (check (length cycles) => 1)
  (check (length (car cycles)) => 3)
) ;let*

;; 自环也被检出。
(let ((cycles (pi-blueprint-detect-cycles (list (task "1.1" '("1.1"))))))
  (check (length cycles) => 1)
) ;let

(check-report)
