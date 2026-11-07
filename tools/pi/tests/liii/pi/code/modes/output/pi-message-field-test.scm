;; pi-message-field
;; 读取消息中的指定字段。
;;
;; 语法
;; ----
;; (pi-message-field msg key default)
;;
;; 参数
;; ----
;; msg : any
;; 消息对象，可为消息记录、alist 或 #f。
;;
;; key : any
;; 字段名，兼容字符串与符号键。
;;
;; default : any
;; 字段缺失时的默认值。
;;
;; 返回值
;; ----
;; any
;; 字段取值，缺失时返回默认值。

(import (liii check)
  (liii pi ai message)
  (liii pi code modes output)
) ;import

(check-set-mode! 'report-failed)

;; 1. alist 消息按字段读取
(let ((msg (list (cons "stopReason" "error") (cons "errorMessage" "boom"))))
  (check (pi-message-field msg "stopReason" #f) => "error")
  (check (pi-message-field msg 'stopReason #f) => "error")
  (check (pi-message-field msg "missing" "fallback") => "fallback")
) ;let*

;; 2. 非消息对象返回默认值
(check (pi-message-field #f "stopReason" #f) => #f)

(check-report)
