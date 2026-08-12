(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-concatenate-reverse
;; 将字符串列表按逆序连接成一个字符串。
;;
;; 语法
;; ----
;; (string-concatenate-reverse string-list [final-string end])
;;
;; 参数
;; ----
;; string-list : list of string?
;; 要连接的字符串列表
;;
;; final-string : string (可选)
;; 最后附加的字符串，默认为空字符串
;;
;; end : integer (可选)
;; final-string 的结束位置（字符索引），默认为 final-string 长度
;;
;; 返回值
;; ------
;; string?
;; 逆序连接后的新字符串
;;
;; 说明
;; ----
;; 1. 先反转列表顺序，再连接
;; 2. 支持可选的 final-string 和 end 参数
;; 3. 性能：O(n)，n 为所有字符串总字节长度
;; 4. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确连接
;;
;; 相关实现
;; --------
;; (liii string) 库中也提供了 string-concatenate-reverse 函数
;; 参见: gf doc liii/string "string-concatenate-reverse"

(check (string-concatenate-reverse '("a" "b" "c")) => "cba")
(check (string-concatenate-reverse '()) => "")
(check (string-concatenate-reverse '("a" "b" "c") "xyz" 2) => "cbaxy")

;; Unicode 测试
(check (string-concatenate-reverse '("a" "b") "中文" 1) => "ba中")

;; Emoji 测试
(check (string-concatenate-reverse '("hello" "😀") "world") => "😀helloworld")
(check (string-concatenate-reverse '("🎉" "🚀")) => "🚀🎉")

(check-report)
