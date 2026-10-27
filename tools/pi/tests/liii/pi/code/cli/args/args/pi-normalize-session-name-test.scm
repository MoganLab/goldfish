(import (liii check) (liii pi code cli args))

(check-set-mode! 'report-failed)

;; pi-normalize-session-name
;; 规范化会话名称，将连续 CR/LF 折叠为一个空格并去除首尾空白。
;;
;; 语法
;; ----
;; (pi-normalize-session-name name) -> string | #f
;;
;; 参数
;; ----
;; name : any
;; 待规范化的名称；只有字符串会被处理。
;;
;; 返回值
;; ----
;; string | #f
;; 返回规范化后的非空名称；非字符串或规范化后为空时返回 #f。
;;
;; 示例
;; ----
;; (pi-normalize-session-name "Fix\nWindows\r\nsupport") => "Fix Windows support"

(check (pi-normalize-session-name "  My session  ") => "My session")
(check (pi-normalize-session-name "Fix\nWindows\r\nsupport")
  =>
  "Fix Windows support"
) ;check
(check (pi-normalize-session-name "first\n\n\nsecond") => "first second")
(check (pi-normalize-session-name " \n\r ") => #f)
(check (pi-normalize-session-name 123) => #f)

(check-report)
