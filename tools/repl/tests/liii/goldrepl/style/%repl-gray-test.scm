;; %repl-gray
;; 用灰色样式包裹文本。
;;
;; 语法
;; ----
;; (%repl-gray text)
;;
;; 参数
;; ----
;; text : string
;; 需要着色的文本。
;;
;; 返回值
;; ----
;; string
;; 带灰色 ANSI 控制序列的文本。

(import (scheme base) (liii check) (liii string) (liii goldrepl style))

(check-set-mode! 'report-failed)

;; 灰色样式等价于 %repl-styled "gray"
(check (%repl-gray "oops") => "\x1b;[90moops\x1b;[0m")
(check (%repl-gray "oops") => (%repl-styled "gray" "oops"))

;; 空文本
(check (%repl-gray "") => "\x1b;[90m\x1b;[0m")

(check-report)
