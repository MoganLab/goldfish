(import (liii check)
        (liii error)
        (liii either)
) ;import

(check-set-mode! 'report-failed)

;; to-left
;; 从 Left 状态的 Either 中提取内部值。
;;
;; 语法
;; ----
;; (to-left either)
;;
;; 参数
;; ----
;; either : either
;; 输入 Either 值。
;;
;; 返回值
;; ----
;; any?
;; 若输入为 Left，则返回其中存放的值。
;;
;; 注意
;; ----
;; 对非 Either 或 Right 调用时会报错。
;;
;; 示例
;; ----
;; (to-left (from-left "error")) => "error"
;;
;; 错误处理
;; ----
;; type-error 当输入不是 Either 时
;; value-error 当输入是 Right 时

(check (to-left (from-left "error message")) => "error message")
(check (to-left (from-left 42)) => 42)
(check (to-left (from-left '())) => '())

(check-catch 'type-error (to-left "not-either"))
(check-catch 'type-error (to-left 123))
(check-catch 'value-error (to-left (from-right "I am Right")))

(check-report)
