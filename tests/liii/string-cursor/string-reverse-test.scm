(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-reverse
;; 反转字符串中的字符顺序。
;;
;; 语法
;; ----
;; (string-reverse s [start end])
;;
;; 参数
;; ----
;; s : string
;; 要反转的字符串
;;
;; start : integer (可选)
;; 反转起始位置（字符索引），默认为0
;;
;; end : integer (可选)
;; 反转结束位置（字符索引），默认为字符串字符数
;;
;; 返回值
;; ------
;; string?
;; 反转后的新字符串
;;
;; 说明
;; ----
;; 1. 返回新字符串，不修改原字符串
;; 2. 支持 Unicode 字符，包括中文和 emoji
;; 3. 支持可选的 start/end 参数指定子串反转
;; 4. 性能：O(n)，n 为子串字符数

;; 基本测试
(check (string-reverse "abc") => "cba")
(check (string-reverse "") => "")
(check (string-reverse "abcdef" 1 4) => "dcb")

;; Unicode 测试
(check (string-reverse "中文") => "文中")
(check (string-reverse "🎉🎊") => "🎊🎉")


;; 测试使用游标作为 start/end
(let* ((s "abcdef") (start (string-cursor-start s)) (end (string-cursor-end s)))
  (check (string-reverse s start end) => "fedcba")
) ;let*
(check-report)
