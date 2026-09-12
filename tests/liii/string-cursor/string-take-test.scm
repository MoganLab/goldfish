(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-take
;; 取字符串前nchars个字符。
;;
;; 语法
;; ----
;; (string-take s nchars)
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
;; 前nchars个字符组成的新字符串
;;
;; 说明
;; ----
;; 1. nchars为0时返回空字符串
;; 2. nchars等于字符串长度时返回原字符串
;; 3. 适用于ASCII、中文、emoji等各种Unicode字符
;; 4. 性能：O(n)，n 为取用的字符数
;; 5. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确截取
;;
;; 相关实现
;; --------
;; (liii string) 库中也提供了 string-take 函数
;; 参见: gf doc liii/string "string-take"
;;
;; 错误处理
;; --------
;; value-error
;; 当nchars大于字符串长度时抛出错误

;; 基本测试 - ASCII
(check (string-take "abcdef" 0) => "")
(check (string-take "abcdef" 2) => "ab")
(check (string-take "abcdef" 6) => "abcdef")

;; 测试中文
(check (string-take "我是中国人" 2) => "我是")
(check (string-take "a中b文c" 4) => "a中b文")

;; 测试emoji
(check (string-take "🎉🎊🎁" 2) => "🎉🎊")

;; 越界检查
(check-catch 'value-error (string-take "abc" 4))

(check-report)
