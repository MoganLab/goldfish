(import (liii check) (liii json schema error) (liii json schema data))

(check-set-mode! 'report-failed)

;; value->display
;; 把值转换为显示文本。
;;
;; 语法
;; ----
;; (value->display value)
;;
;; 参数
;; ----
;; value : any?
;; 待转换的值。
;;
;; 返回值
;; ----
;; string
;;
;; 注意
;; ----
;; 值的外部表示文本。
;; 用于错误消息中的值渲染。
;;
;; 示例
;; ----
;; (value->display 1) => "1"
;;
;; 错误处理
;; ----
;; 无。

(check (value->display 1) => "1")
(check (value->display "x") => "\"x\"")
(check (value->display 'null) => "null")
(check-true
  (string? (value->display '(("a" . 1))))
) ;check-true

(check-report)
