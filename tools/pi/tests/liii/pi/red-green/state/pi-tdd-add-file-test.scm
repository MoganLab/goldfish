(import (liii check)
        (liii pi red-green state)
) ;import

(check-set-mode! 'report-failed)

;; pi-tdd-add-test-file / pi-tdd-add-impl-file / pi-tdd-stale-test-run?
;; 文件历史去重、测试运行时间戳与陈旧判定。

(let* ((s (pi-tdd-initial-state "task"))
       (s1 (pi-tdd-add-test-file s "test_calc.py"))
       (s2 (pi-tdd-add-test-file s1 "test_calc.py"))
       (s3 (pi-tdd-add-impl-file s2 "calc.py"))
       (s4 (pi-tdd-add-impl-file s3 "calc.py"))
      ) ;
  (check (pi-tdd-state-test-files s1) => '("test_calc.py"))
  (check (pi-tdd-state-test-files s2) => '("test_calc.py"))
  (check (pi-tdd-state-impl-files s3) => '("calc.py"))
  (check (pi-tdd-state-impl-files s4) => '("calc.py"))
) ;let*

;; 未运行过测试：不算陈旧。
(check (pi-tdd-stale-test-run? (pi-tdd-initial-state "task")) => #f)

;; 同一回合运行：不陈旧；下一回合：陈旧。
(let* ((s (pi-tdd-set-turn-index (pi-tdd-initial-state "task") 1))
       (run (list (cons 'passed 1)
              (cons 'failed 0)
              (cons 'errors 0)
              (cons 'exit-code 0)
              (cons 'turn-index 1)
            ) ;list
       ) ;run
       (s1 (pi-tdd-set-last-test-run s run))
       (s2 (pi-tdd-set-turn-index s1 2))
      ) ;
  (check (pi-tdd-stale-test-run? s1) => #f)
  (check (pi-tdd-stale-test-run? s2) => #t)
  (check (cdr (assoc 'passed (pi-tdd-state-last-test-run s2))) => 1)
) ;let*

;; set-warning 不改变其他字段。
(let* ((s (pi-tdd-initial-state "task"))
       (w (pi-tdd-set-warning s "warning"))
      ) ;
  (check (pi-tdd-state-warning w) => "warning")
  (check (pi-tdd-state-phase w) => 'red)
  (check (pi-tdd-state-task w) => "task")
) ;let*

(check-report)
