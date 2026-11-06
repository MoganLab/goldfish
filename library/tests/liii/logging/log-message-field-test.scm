;; log-message-field
;; 按键读取日志消息中的附加字段。
;;
;; 语法
;; ----
;; (log-message-field msg key)
;;
;; 参数
;; ----
;; msg : list
;; 日志 alist。
;; key : symbol
;; 字段名。
;;
;; 返回值
;; ----
;; any 或 #f
;; 字段值；字段不存在时返回 #f。

(import (liii check) (liii logging))

(check-set-mode! 'report-failed)

(check (log-message-field '((SEVERITY . 3) (CODE . 500)) 'CODE) => 500)
(check (log-message-field '((SEVERITY . 3) (MESSAGE . "err")) 'MESSAGE) => "err")
(check (log-message-field '((SEVERITY . 3)) 'MISSING) => #f)

(check-report)
