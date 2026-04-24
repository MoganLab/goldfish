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
;; 3. 性能：O(n²)，n 为字符串列表长度，因为每次 string-append 都会创建新字符串

(check (string-concatenate-reverse '("a" "b" "c")) => "cba")
(check (string-concatenate-reverse '()) => "")
(check (string-concatenate-reverse '("a" "b" "c") "xyz" 2) => "cbaxy")

;; Unicode 测试
(check (string-concatenate-reverse '("a" "b") "中文" 1) => "ba中")

(check-report)
