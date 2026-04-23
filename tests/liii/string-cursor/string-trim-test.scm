(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-trim
;; 去掉字符串左侧满足谓词的字符。
;;
;; 语法
;; ----
;; (string-trim s [pred start end])
;;
;; 参数
;; ----
;; s : string
;; 要修剪的字符串
;;
;; pred : procedure (可选)
;; 一元字符谓词，默认为 char-whitespace?
;;
;; start : integer (可选)
;; 搜索起始位置，默认为0
;;
;; end : integer (可选)
;; 搜索结束位置，默认为字符串字符数
;;
;; 返回值
;; ------
;; string?
;; 修剪后的字符串

;; 基本测试
(check (string-trim "  abc  ") => "abc  ")
(check (string-trim "abc  ") => "abc  ")
(check (string-trim "  abc") => "abc")
(check (string-trim "abc") => "abc")
(check (string-trim "") => "")
(check (string-trim "   ") => "")

;; 自定义谓词
(check (string-trim "xxabcxx" (lambda (c) (char=? c #\x))) => "abcxx")

;; 测试中文
(check (string-trim "  中文  ") => "中文  ")

;; string-trim-right
;; 去掉字符串右侧满足谓词的字符。
(check (string-trim-right "  abc  ") => "  abc")
(check (string-trim-right "abc  ") => "abc")
(check (string-trim-right "  abc") => "  abc")
(check (string-trim-right "abc") => "abc")
(check (string-trim-right "") => "")
(check (string-trim-right "   ") => "")

;; 自定义谓词
(check (string-trim-right "xxabcxx" (lambda (c) (char=? c #\x))) => "xxabc")

;; 测试中文
(check (string-trim-right "  中文  ") => "  中文")

;; string-trim-both
;; 去掉字符串两侧满足谓词的字符。
(check (string-trim-both "  abc  ") => "abc")
(check (string-trim-both "abc") => "abc")
(check (string-trim-both "  abc") => "abc")
(check (string-trim-both "abc  ") => "abc")
(check (string-trim-both "") => "")
(check (string-trim-both "   ") => "")

;; 自定义谓词
(check (string-trim-both "xxabcxx" (lambda (c) (char=? c #\x))) => "abc")

;; 测试中文
(check (string-trim-both "  中文  ") => "中文")

(check-report)
