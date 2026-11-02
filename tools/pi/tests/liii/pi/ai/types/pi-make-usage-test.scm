;; pi-make-usage
;; 构造 token 用量统计关联列表快捷方式。
;;
;; 语法
;; ----
;; (pi-make-usage)
;; (pi-make-usage input output total)
;;
;; 参数
;; ----
;; input : integer?
;; 可选，输入 token 数量（默认为 0）。
;;
;; output : integer?
;; 可选，输出 token 数量（默认为 0）。
;;
;; total : integer?
;; 可选，总 token 数量（默认为 0）。
;;
;; 返回值
;; ----
;; alist?
;; 包含 input、output 与 total 字段的关联列表。

(import (liii check) (liii pi ai types))

(check-set-mode! 'report-failed)

(let ((u (pi-make-usage 100 200 300)))
  (check-true (pair? u))
  (check (cdr (assoc 'input u)) => 100)
  (check (cdr (assoc 'output u)) => 200)
  (check (cdr (assoc 'total u)) => 300)
) ;let

(check-report)
