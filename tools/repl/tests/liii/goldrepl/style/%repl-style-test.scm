;; %repl-style
;; 按样式名取出对应的 ANSI 控制序列。
;;
;; 语法
;; ----
;; (%repl-style name)
;;
;; 参数
;; ----
;; name : string
;; 样式名，例如 "keyword"、"error"。
;;
;; 返回值
;; ----
;; string
;; 对应的 ANSI 控制序列；样式名不存在时返回空字符串。

(import (scheme base) (liii check) (liii string) (liii goldrepl style))

(check-set-mode! 'report-failed)

;; 已定义的样式
(check (%repl-style "error") => "\x1b;[31m")
(check (%repl-style "keyword") => "\x1b;[35m")
(check (%repl-style "reset") => "\x1b;[0m")

;; 未定义的样式返回空字符串
(check (%repl-style "no-such-style") => "")

(check-report)
