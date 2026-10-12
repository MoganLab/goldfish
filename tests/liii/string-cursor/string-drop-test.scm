(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-drop
;; 跳过字符串前nchars个字符，返回剩余部分。
;;
;; 语法
;; ----
;; (string-drop s nchars)
;;
;; 参数
;; ----
;; s : string
;; 源字符串
;;
;; nchars : integer
;; 要跳过的字符个数
;;
;; 返回值
;; ------
;; string?
;; 跳过前nchars个字符后的剩余字符串
;;
;; 说明
;; ----
;; 1. nchars为0时返回原字符串
;; 2. nchars等于字符串长度时返回空字符串
;; 3. 适用于ASCII、中文、emoji等各种Unicode字符
;; 4. 性能：O(n)，n 为跳过的字符数
;; 5. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确截取
;;
;; 相关实现
;; --------
;; (liii string) 库中也提供了 string-drop 函数
;; 参见: gf doc liii/string "string-drop"
;;
;; 错误处理
;; --------
;; value-error
;; 当nchars大于字符串长度时抛出错误

;; 基本测试 - ASCII
(check (string-drop "abcdef" 0) => "abcdef")
(check (string-drop "abcdef" 2) => "cdef")
(check (string-drop "abcdef" 6) => "")

;; 测试中文
(check (string-drop "我是中国人" 2) => "中国人")

;; 测试emoji
(check (string-drop "🎉🎊🎁" 1) => "🎊🎁")

;; 越界检查
(check-catch 'value-error (string-drop "abc" 4))

(check-report)
