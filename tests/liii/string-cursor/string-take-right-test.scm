(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-take-right
;; 取字符串后nchars个字符。
;;
;; 语法
;; ----
;; (string-take-right s nchars)
;;
;; 参数
;; ----
;; s : string
;; 源字符串
;;
;; nchars : integer
;; 要取的字符个数
;;
;; 返回值
;; ------
;; string?
;; 后nchars个字符组成的新字符串
;;
;; 说明
;; ----
;; 1. nchars为0时返回空字符串
;; 2. nchars等于字符串长度时返回原字符串
;; 3. 适用于ASCII、中文、emoji等各种Unicode字符
;; 4. 性能：O(n)，n 为取用的字符数
;;
;; 错误处理
;; --------
;; value-error
;; 当nchars大于字符串长度时抛出错误

;; 基本测试 - ASCII
(check (string-take-right "abcdef" 0) => "")
(check (string-take-right "abcdef" 2) => "ef")
(check (string-take-right "abcdef" 6) => "abcdef")

;; 测试中文
(check (string-take-right "我是中国人" 2) => "国人")

;; 测试emoji
(check (string-take-right "🎉🎊🎁" 2) => "🎊🎁")

;; 越界检查
(check-catch 'value-error (string-take-right "abc" 4))

(check-report)
