(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-any
;; 检查是否有任意字符满足谓词。
;;
;; 语法
;; ----
;; (string-any pred s [start end])
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
;; 如果有字符满足谓词，返回第一个满足谓词的结果；否则返回 #f
;; 空字符串返回 #f
;;
;; 说明
;; ----
;; 1. string-any 是 SRFI-130 中的字符串检查函数
;; 2. 与 (liii string) 中的 string-any 功能相同，但本版本使用 cursor 遍历字符串
;; 3. start/end 参数可以是整数索引或 string-cursor
;; 4. 与 (liii string) 版本的差异：本版本只接受谓词(procedure)，不支持字符参数
;;
;; 相关实现
;; --------
;; (liii string) 库中也提供了 string-any 函数，该版本支持 char/pred?
;; 两种参数类型，基于整数索引遍历。
;; 参见: gf doc liii/string "string-any"
;; 性能：O(n)，n 为检查的字符数

;; 空字符串返回 #f
(check (string-any char? "") => #f)

;; 有字符满足
(check (string-any char-numeric? "abc123") => #t)
(check (string-any char-numeric? "123") => #t)

;; 没有字符满足
(check (string-any char-numeric? "abc") => #f)

;; 返回第一个满足谓词的结果
(check (string-any (lambda (c) (if (char-numeric? c) c #f)) "abc123") => #\1)

;; 测试中文字符
(check (string-any char? "中文") => #t)

;; 测试 emoji 字符
(check (string-any (lambda (c) (char=? c #\😀)) "hello😀world") => #t)
(check (string-any (lambda (c) (char=? c #\😀)) "hello world") => #f)

;; 测试使用游标作为 start/end
(let* ((s "abc123")
       (start (string-cursor-start s))
       (end (string-cursor-end s)))
  (check (string-any char-numeric? s start end) => #t))
(check-report)
