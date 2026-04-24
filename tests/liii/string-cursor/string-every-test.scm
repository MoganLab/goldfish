(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-every
;; 检查是否所有字符都满足谓词。
;;
;; 语法
;; ----
;; (string-every pred s [start end])
;;
;; 参数
;; ----
;; pred : procedure
;; 一元字符谓词
;;
;; s : string
;; 要检查的字符串
;;
;; start, end : integer 或 string-cursor? (可选)
;; 子串范围，默认为整个字符串
;;
;; 返回值
;; ------
;; boolean? 或任意值
;; 如果所有字符都满足谓词，返回最后一个满足谓词的结果；否则返回 #f
;; 空字符串返回 #t
;;
;; 说明
;; ----
;; 1. string-every 是 SRFI-130 中的字符串检查函数
;; 2. 与 (liii string) 中的 string-every 功能相同
;; 3. 性能：O(n)，n 为检查的字符数

;; 空字符串返回 #t
(check (string-every char? "") => #t)

;; 所有字符都满足
(check (string-every char? "abc") => #t)
(check (string-every char-alphabetic? "abc") => #t)

;; 不是所有字符都满足
(check (string-every char-alphabetic? "abc123") => #f)

;; 返回最后一个满足谓词的结果
(check (string-every (lambda (c) (if (char-alphabetic? c) c #f)) "abc") => #\c)

;; 测试中文字符
(check (string-every char? "中文") => #t)
(check (string-every (lambda (c) (if (char>? c #\a) c #f)) "bcd") => #\d)

;; 测试使用游标作为 start/end
(let* ((s "abcdef")
       (start (string-cursor-start s))
       (end (string-cursor-end s)))
  (check (string-every char-alphabetic? s start end) => #t))

(check-report)
