(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-filter
;; 保留字符串中满足谓词的字符。
;;
;; 语法
;; ----
;; (string-filter pred s [start end])
;;
;; 参数
;; ----
;; pred : procedure
;; 一元字符谓词函数
;;
;; s : string
;; 源字符串
;;
;; start : integer (可选)
;; 起始位置，默认为0
;;
;; end : integer (可选)
;; 结束位置，默认为字符串字符数
;;
;; 返回值
;; ------
;; string?
;; 过滤后的新字符串
;;
;; 说明
;; ----
;; 1. 返回新字符串，不修改原字符串
;; 2. 与 (liii string) 的区别：(liii string-cursor) 按字符过滤，支持 Unicode
;; 3. 性能：O(n)，只需遍历一次字符串
;; 4. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确过滤
;;
;; 相关实现
;; --------
;; (liii string) 库中也提供了 string-filter 函数
;; 参见: gf doc liii/string "string-filter"

(check (string-filter char-alphabetic? "abc123") => "abc")
(check (string-filter char-numeric? "abc123") => "123")
(check (string-filter char-alphabetic? "中文123") => "中文")


;; 测试使用游标作为 start/end
(let* ((s "abc123") (start (string-cursor-start s)) (end (string-cursor-end s)))
  (check (string-filter char-alphabetic? s start end) => "abc")
) ;let*
(check-report)
