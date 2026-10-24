(import (liii check)
        (srfi srfi-166 base)
        (srfi srfi-166 unicode))

(check-set-mode! 'report-failed)

;; (srfi srfi-166 unicode) 模块函数分类索引
;;
;; unicode 模块提供终端宽度感知、中日韩全角字符排版与大小写变换组合子。
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc srfi/srfi-166/unicode "terminal-aware"
;;   bin/gf doc srfi/srfi-166/unicode "string-terminal-width"
;;   bin/gf doc srfi/srfi-166/unicode "upcased"

(check (string-terminal-width "日本語") => 6)
(check (show #f (terminal-aware (trimmed 2 "日本語"))) => "語")
(check (show #f (upcased "abc")) => "ABC")

(check-report)
