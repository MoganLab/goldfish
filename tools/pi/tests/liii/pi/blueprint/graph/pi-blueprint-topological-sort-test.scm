(import (liii check)
        (liii pi blueprint types)
        (liii pi blueprint graph)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "desc" '() '() deps)
) ;define

;; pi-blueprint-topological-sort
;; 依赖在前，阶段内顺序稳定。
(check (pi-blueprint-topological-sort
         (list (task "1.2" '("1.1")) (task "1.1" '()))
       ) ;pi-blueprint-topological-sort
  =>
  '("1.1" "1.2")
) ;check
(check (pi-blueprint-topological-sort
         (list (task "2.1" '("1.1")) (task "1.1" '()))
       ) ;pi-blueprint-topological-sort
  =>
  '("1.1" "2.1")
) ;check
(check (pi-blueprint-topological-sort (list (task "1.1" '()))) => '("1.1"))

;; 环中节点仍全部返回（Kahn 剩余部分不出现），顺序只覆盖可排序节点。
(check (length (pi-blueprint-topological-sort
                 (list (task "1.1" '("1.2")) (task "1.2" '("1.1")))
               ) ;pi-blueprint-topological-sort
       ) ;length
  =>
  0
) ;check

(check-report)
