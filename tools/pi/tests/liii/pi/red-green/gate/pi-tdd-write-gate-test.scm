(import (liii check)
        (liii string)
        (liii pi red-green state)
        (liii pi red-green gate)
) ;import

(check-set-mode! 'report-failed)

(define (args path)
  (list (cons "path" path))
) ;define

(define (red-state)
  (pi-tdd-initial-state "task")
) ;define

(define (green-state)
  (let-values (((next warning) (pi-tdd-advance-phase (red-state) 'tests-fail)))
    next
  ) ;let-values
) ;define

;; pi-tdd-write-gate
;; RED 阶段拒绝实现文件写入，测试文件与其他文件放行。

;; 无活动 TDD 会话：不拦截。
(pi-tdd-set-current-state! #f)
(check (pi-tdd-write-gate "edit" (args "calc.py")) => #f)

;; RED + 实现文件：拒绝，并给出可读提示。
(pi-tdd-set-current-state! (red-state))
(let ((msg (pi-tdd-write-gate "edit" (args "calc.py"))))
  (check-true (string? msg))
  (check-true (string-contains? msg "TDD RED phase violation"))
  (check-true (string-contains? msg "calc.py"))
  (check-true (string-contains? msg "Write a failing test first"))
) ;let
(let ((msg (pi-tdd-write-gate "write" (args "src/new-feature.ts"))))
  (check-true (string? msg))
) ;let

;; RED + 测试文件：放行。
(check (pi-tdd-write-gate "edit" (args "test_calc.py")) => #f)
(check (pi-tdd-write-gate "write" (args "src/calc.test.ts")) => #f)

;; RED + 配置文件：放行。
(check (pi-tdd-write-gate "write" (args "package.json")) => #f)

;; RED + 非写工具：不拦截。
(check (pi-tdd-write-gate "read" (args "calc.py")) => #f)
(check (pi-tdd-write-gate "bash" (args "calc.py")) => #f)

;; GREEN 阶段：实现文件放行。
(pi-tdd-set-current-state! (green-state))
(check (pi-tdd-write-gate "edit" (args "calc.py")) => #f)

;; 缺少 path 参数：不拦截（由工具自身报错）。
(pi-tdd-set-current-state! (red-state))
(check (pi-tdd-write-gate "edit" '()) => #f)

;; pi-tdd-note-edited-file!
;; 成功写入后记录文件历史；失败写入不记录。

(pi-tdd-set-current-state! (green-state))
(pi-tdd-note-edited-file! "edit" (args "calc.py") #f)
(pi-tdd-note-edited-file! "write" (args "test_calc.py") #f)
(check (pi-tdd-state-impl-files (pi-tdd-current-state)) => '("calc.py"))
(check (pi-tdd-state-test-files (pi-tdd-current-state)) => '("test_calc.py"))

(pi-tdd-set-current-state! (green-state))
(pi-tdd-note-edited-file! "edit" (args "calc.py") #t)
(check (pi-tdd-state-impl-files (pi-tdd-current-state)) => '())

;; other 类型不记录。
(pi-tdd-set-current-state! (green-state))
(pi-tdd-note-edited-file! "write" (args "README.md") #f)
(check (pi-tdd-state-impl-files (pi-tdd-current-state)) => '())
(check (pi-tdd-state-test-files (pi-tdd-current-state)) => '())

(pi-tdd-set-current-state! #f)

(check-report)
