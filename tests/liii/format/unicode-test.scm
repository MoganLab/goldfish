(import (liii check)
        (liii format base)
        (liii format unicode))

(check-set-mode! 'report-failed)

;; (liii format unicode) 模块函数分类索引
;;
;; unicode 模块提供终端宽度感知、中日韩全角字符排版与大小写变换组合子。
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/format/unicode "terminal-aware"
;;   bin/gf doc liii/format/unicode "string-terminal-width"
;;   bin/gf doc liii/format/unicode "upcased"

(check (string-terminal-width "日本語") => 6)
(check (show #f (terminal-aware (trimmed 2 "日本語"))) => "語")
(check (show #f (upcased "abc")) => "ABC")

(check-report)
