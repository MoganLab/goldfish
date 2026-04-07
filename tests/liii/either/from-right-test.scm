(import (liii check)
        (liii error)
        (liii either)
) ;import

(check-set-mode! 'report-failed)

;; from-right
;; 创建 Right 值，通常代表成功或有效数据。
;;
;; 语法
;; ----
;; (from-right value)
;;
;; 参数
;; ----
;; value : any?
;; 要包装到 Right 中的值。
;;
;; 返回值
;; ----
;; either
;; 一个 Right 状态的 Either 值。
;;
;; 注意
;; ----
;; 本文件只验证构造结果本身，不覆盖提取函数行为。
;;
;; 示例
;; ----
;; (from-right 100) => 一个 Right 状态的 Either 值
;;
;; 错误处理
;; ----
;; 无

(let ((v (from-right "success data")))
  (check-true (pair? v))
  (check (car v) => "success data")
  (check (cdr v) => 'right)
) ;let

(let ((v (from-right 100)))
  (check (car v) => 100)
  (check (cdr v) => 'right)
) ;let

(let ((v (from-right '(1 2 3))))
  (check (car v) => '(1 2 3))
  (check (cdr v) => 'right)
) ;let

(check-report)
