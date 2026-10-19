(import (liii check) (liii goldfix))

(check-set-mode! 'report-failed)

;; repair-source 是 gf fix 的安全包装：fix 是缩进驱动的，括号本已平衡时
;; 任何按缩进的改动都只会破坏结构，因此拒绝改动、原样返回。

;; 1) 括号已平衡 + 缩进错乱（未先 gf fmt）：必须原样返回，不能拆散结构。
;;    （此情形会向 stdout 打印"请先运行 gf fmt"的提示。）
(check (repair-source "(define (f x)\n(let ((y 2))\n(+ x y)))\n")
  =>
  "(define (f x)\n(let ((y 2))\n(+ x y)))\n"
) ;check

;; 2) 括号已平衡 + 缩进正常：本就无需改动，原样返回（行为不变）。
(check (repair-source "(define (f x)\n  (let ((y 2))\n    (+ x y)))\n")
  =>
  "(define (f x)\n  (let ((y 2))\n    (+ x y)))\n"
) ;check

;; 3) 括号缺失（不平衡）：守卫不介入，正常补全括号。
(check (repair-source "(define (f x)\n  (+ x 1)")
  =>
  "(define (f x)\n  (+ x 1))"
) ;check

;; 4) 括号多余（不平衡）：守卫不介入，正常删除多余括号。
(check (repair-source "(define x 1))") => "(define x 1)")

;; repair-source* 返回 (repaired skipped?)，让调用方区分两种"未改动"。
;; 5) 平衡 + 缩进乱：skipped? 为 #t（守卫拦截，原样返回）。
(check (call-with-values
         (lambda () (repair-source* "(define (f x)\n(let ((y 2))\n(+ x y)))\n"))
         (lambda (repaired skipped?) skipped?))
  => #t
) ;check

;; 6) 平衡 + 缩进正常：skipped? 为 #f（本就平衡，无需改）。
(check (call-with-values
         (lambda () (repair-source* "(define (f x)\n  (let ((y 2))\n    (+ x y)))\n"))
         (lambda (repaired skipped?) skipped?))
  => #f
) ;check

;; 7) 缺括号（不平衡）：skipped? 为 #f（正常修复，守卫不介入）。
(check (call-with-values
         (lambda () (repair-source* "(define (f x)\n  (+ x 1)"))
         (lambda (repaired skipped?) skipped?))
  => #f
) ;check

(check-report)
