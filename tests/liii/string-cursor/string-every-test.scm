(import (liii check))
(import (scheme char))
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
;; 2. 与 (liii string) 中的 string-every 功能相同，但本版本使用 cursor 遍历字符串
;; 3. start/end 参数可以是整数索引或 string-cursor
;; 4. 与 (liii string) 版本的差异：本版本只接受谓词(procedure)，不支持字符参数
;; 5. 支持 Unicode 字符（包括多字节字符如中文、Emoji）的正确遍历和检查
;;
;; 相关实现
;; --------
;; (liii string) 库中也提供了 string-every 函数，该版本支持 char/pred?
;; 两种参数类型，基于整数索引遍历。
;; 参见: gf doc liii/string "string-every"
;; 性能：O(n)，n 为检查的字符数

;; 空字符串返回 #t
(check (string-every char? "") => #t)

;; 所有字符都满足
(check (string-every char-alphabetic? "abc") => #t)

;; 不是所有字符都满足
(check (string-every char-alphabetic? "abc123") => #f)

;; 返回最后一个满足谓词的结果
(check (string-every (lambda (c) (if (char-alphabetic? c) c #f)) "abc") => #\c)

;; 测试中文字符
(check (string-every char? "中文") => #t)

;; 测试 emoji 字符
(check (string-every (lambda (c) (char=? c #\😀)) "😀😀😀") => #t)
(check (string-every (lambda (c) (char=? c #\😀)) "😀a😀") => #f)

;; 测试使用游标作为 start/end
(let* ((s "abcdef") (start (string-cursor-start s)) (end (string-cursor-end s)))
  (check (string-every char-alphabetic? s start end) => #t)
) ;let*

(check-report)
