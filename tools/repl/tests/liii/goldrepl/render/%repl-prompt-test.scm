;; %repl-prompt
;; 返回首行提示符或续行提示符。
;;
;; 语法
;; ----
;; (%repl-prompt first?)
;;
;; 参数
;; ----
;; first? : boolean
;; 为真时返回首行提示符，否则返回续行提示符。
;;
;; 返回值
;; ----
;; string
;; 带提示符样式的提示符文本。

(import (scheme base) (liii check) (liii string) (liii goldrepl render))

(check-set-mode! 'report-failed)

;; 首行提示符
(check (%repl-prompt #t) => "\x1b;[33mgf> \x1b;[0m")

;; 续行提示符
(check (%repl-prompt #f) => "\x1b;[33m... \x1b;[0m")

(check-report)
