(import (liii check)
  (liii error)
  (liii either)
) ;import

(check-set-mode! 'report-failed)

;; from-left
;; 创建 Left 值，通常代表错误或异常情况。
;;
;; 语法
;; ----
;; (from-left value)
;;
;; 参数
;; ----
;; value : any?
;; 要包装到 Left 中的值。
;;
;; 返回值
;; ----
;; either
;; 一个 Left 状态的 Either 值。
;;
;; 注意
;; ----
;; 本文件只验证构造结果本身，不覆盖提取函数行为。
;;
;; 示例
;; ----
;; (from-left "error") => 一个 Left 状态的 Either 值
;;
;; 错误处理
;; ----
;; 无

(let ((v (from-left "error message")))
  (check-true (pair? v))
  (check (car v) => "error message")
  (check (cdr v) => 'left)
) ;let

(let ((v (from-left 42)))
  (check (car v) => 42)
  (check (cdr v) => 'left)
) ;let

(let ((v (from-left '())))
  (check (car v) => '())
  (check (cdr v) => 'left)
) ;let

(check-report)
