;; (scheme r5rs) 模块文档与测试
;;
;; `(scheme r5rs)` 提供 R5RS 报告中定义的标识符（不含 transcript-on /
;; transcript-off），其中 exact/inexact 以 R5RS 名字 inexact->exact /
;; exact->inexact 导出。
;;
;; ==== 说明 ====
;;
;; 1. 该库是 R5RS 兼容层，重新导出基础库、字符库、复数库等标准库的绑定
;; 2. scheme-report-environment 返回包含 R5RS 全部绑定的环境
;; 3. null-environment 返回仅包含语法绑定的环境
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/r5rs
(import (liii check) (scheme r5rs) (scheme eval))
(check-set-mode! 'report-failed)

;; ==== 测试：R5RS 过程可用 ====
(check (procedure? car) => #t)
(check (procedure? cdr) => #t)
(check (procedure? inexact->exact) => #t)
(check (procedure? exact->inexact) => #t)
(check (procedure? force) => #t)
(check (procedure? eval) => #t)
(check (procedure? load) => #t)
(check (procedure? interaction-environment) => #t)
(check (procedure? display) => #t)
(check (procedure? write) => #t)
(check (procedure? read) => #t)
(check (procedure? char-ci=?) => #t)
(check (procedure? string-ci=?) => #t)
(check (procedure? make-polar) => #t)
(check (procedure? cadddr) => #t)

;; ==== 测试：R5RS 数值语义 ====
(check (inexact->exact 3.5) => 7/2)
(check (= (exact->inexact 3) 3.0) => #t)

;; ==== 测试：scheme-report-environment ====
(let ((env (scheme-report-environment 5)))
  (check (not (null? env)) => #t)
  (check (eval '(+ 40 2) env) => 42)
) ;let

;; ==== 测试：null-environment ====
(let ((env (null-environment 5)))
  (check (not (null? env)) => #t)
) ;let

(check-report)
