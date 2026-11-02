;; form-line
;; 获取 S 表达式对应的源码行号（非 pair 形式或无行号时返回默认值 1）。
;;
;; 语法
;; ----
;; (form-line form)
;;
;; 参数
;; ----
;; form : any  S 表达式形式
;;
;; 返回值
;; ------
;; integer
;; 源码行号整数（>= 1）。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 1. 非 pair 形式返回 1
(check (form-line 'symbol) => 1)
(check (form-line 123) => 1)

;; 2. pair 形式返回整数（>= 1）
(let ((line (form-line '(define x 1))))
  (check-true (integer? line))
  (check-true (>= line 1))
) ;let

(check-report)
