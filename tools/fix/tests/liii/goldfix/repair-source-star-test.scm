;; repair-source*
;; 修正源码中缺失或多余的右括号，返回 (修正后源码 是否跳过)。
;; skipped? 为 #t 表示括号本已平衡但缩进不可信，守卫拦截后原样返回，
;; 调用方可据此区分"本就平衡无需改"与"被守卫拒绝"。
;;
;; 语法
;; ----
;; (repair-source* source)
;;
;; 参数
;; ----
;; source : string? 源码文本
;;
;; 返回值
;; ------
;; values
;; 第一个值为修正后的源码字符串，第二个值为是否被守卫拦截的标志。

(import (liii check)
  (liii base)
  (liii goldfix)
) ;import

(check-set-mode! 'report-failed)

;; 测试：平衡 + 缩进乱：skipped? 为 #t（守卫拦截，原样返回）
(check (call-with-values
         (lambda () (repair-source* "(define (f x)\n(let ((y 2))\n(+ x y)))\n"))
         (lambda (repaired skipped?) skipped?))
  => #t
) ;check

;; 测试：平衡 + 缩进正常：skipped? 为 #f（本就平衡，无需改）
(check (call-with-values
         (lambda () (repair-source* "(define (f x)\n  (let ((y 2))\n    (+ x y)))\n"))
         (lambda (repaired skipped?) skipped?))
  => #f
) ;check

;; 测试：缺括号（不平衡）：skipped? 为 #f（正常修复，守卫不介入）
(check (call-with-values
         (lambda () (repair-source* "(define (f x)\n  (+ x 1)"))
         (lambda (repaired skipped?) skipped?))
  => #f
) ;check

(check-report)
