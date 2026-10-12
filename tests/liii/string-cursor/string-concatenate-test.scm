(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-concatenate
;; 将字符串列表连接成一个字符串。
;;
;; 语法
;; ----
;; (string-concatenate string-list)
;;
;; 参数
;; ----
;; string-list : list of string?
;; 要连接的字符串列表
;;
;; 返回值
;; ------
;; string?
;; 连接后的新字符串
;;
;; 说明
;; ----
;; 1. 列表为空时返回空字符串
;; 2. 支持 Unicode 字符串
;; 3. 性能：O(n)，n 为所有字符串总字节长度
;; 4. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确连接
;;
;; 相关实现
;; --------
;; (liii string) 库中也提供了 string-concatenate 函数
;; 参见: gf doc liii/string "string-concatenate"

(check (string-concatenate '("a" "b" "c")) => "abc")
(check (string-concatenate '()) => "")
(check (string-concatenate '("中文" "测试")) => "中文测试")

;; Emoji 测试
(check (string-concatenate '("hello" "😀" "world")) => "hello😀world")
(check (string-concatenate '("🎉" "🚀")) => "🎉🚀")

(check-report)
