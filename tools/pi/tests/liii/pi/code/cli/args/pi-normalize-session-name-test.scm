;; pi-normalize-session-name
;; 规范化会话显示名：换行折叠为空格、去首尾空白；空名或非字符串返回 #f。
;;
;; 语法
;; ----
;; (pi-normalize-session-name name) -> string | #f
;;
;; 参数
;; ----
;; name : any
;; 原始会话名。
;;
;; 返回值
;; ------
;; string | #f
;; 规范化后的名称；无效输入为 #f。

(import (scheme base)
  (liii check)
  (liii pi code cli args)
) ;import

(check-set-mode! 'report-failed)

(check (pi-normalize-session-name "simple-name") => "simple-name")

;; 换行折叠为空格并去首尾空白
(check (pi-normalize-session-name "  multi\nline\rname  ") => "multi line name")

;; 空串与空白返回 #f
(check-false (pi-normalize-session-name ""))
(check-false (pi-normalize-session-name "   "))

;; 非字符串返回 #f
(check-false (pi-normalize-session-name 42))

(check-report)
