(import (liii check)
        (srfi srfi-166 base)
        (srfi srfi-166 color))

(check-set-mode! 'report-failed)

;; as-bold
;; 终端文本修饰组合子。为文本添加粗体、斜体或下划线样式，并在结束时恢复。
;;
;; 语法
;; ----
;; (as-bold fmt ...)
;; (as-italic fmt ...)
;; (as-underline fmt ...)
;;
;; 示例
;; ----
;; (show #f (as-bold "bold")) => "\x1b[1mbold\x1b[22m"

(check (show #f (as-bold "bold")) => "\x1b[1mbold\x1b[22m")
(check (show #f (as-italic "italic")) => "\x1b[3mitalic\x1b[23m")
(check (show #f (as-underline "underline")) => "\x1b[4munderline\x1b[24m")
(check (show #f (as-red (each (as-underline (as-bold "abc") "def") "ghi")))
       => "\x1b[31m\x1b[4m\x1b[1mabc\x1b[22mdef\x1b[24mghi\x1b[39m")

(check-report)
