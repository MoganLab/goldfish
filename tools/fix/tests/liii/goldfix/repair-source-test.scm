;; repair-source
;; 修正源码中缺失或多余的右括号，只返回修正后的源码。
;; fix 是缩进驱动的：括号本已平衡时任何按缩进的改动都只会破坏结构，
;; 因此拒绝改动、原样返回。
;;
;; 语法
;; ----
;; (repair-source source)
;;
;; 参数
;; ----
;; source : string? 源码文本
;;
;; 返回值
;; ------
;; string? 修正后的源码文本

(import (liii check)
  (liii base)
  (liii goldfix)
) ;import

(check-set-mode! 'report-failed)

;; 测试：括号已平衡 + 缩进错乱（未先 gf fmt）：必须原样返回，不能拆散结构
(check (repair-source "(define (f x)\n(let ((y 2))\n(+ x y)))\n")
  =>
  "(define (f x)\n(let ((y 2))\n(+ x y)))\n"
) ;check

;; 测试：括号已平衡 + 缩进正常：本就无需改动，原样返回
(check (repair-source "(define (f x)\n  (let ((y 2))\n    (+ x y)))\n")
  =>
  "(define (f x)\n  (let ((y 2))\n    (+ x y)))\n"
) ;check

;; 测试：括号缺失（不平衡）：守卫不介入，正常补全括号
(check (repair-source "(define (f x)\n  (+ x 1)")
  =>
  "(define (f x)\n  (+ x 1))"
) ;check

;; 测试：括号多余（不平衡）：守卫不介入，正常删除多余括号
(check (repair-source "(define x 1))") => "(define x 1)")

(check-report)
