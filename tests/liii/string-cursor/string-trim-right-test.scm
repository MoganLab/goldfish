(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-trim-right
;; 去掉字符串右侧满足谓词的字符。
;;
;; 语法
;; ----
;; (string-trim-right s [pred start end])
;;
;; 参数
;; ----
;; s : string
;; 要修剪的字符串
;;
;; pred : procedure (可选)
;; 一元字符谓词，默认为 char-whitespace?
;;
;; start, end : integer 或 string-cursor? (可选)
;; 子串范围，默认为整个字符串
;;
;; 返回值
;; ------
;; string?
;; 修剪后的字符串
;;
;; 说明
;; ----
;; 1. string-trim-right 是 SRFI-130 中的字符串修剪函数
;; 2. 与 (liii string) 中的 string-trim-right 功能相同
;; 3. 性能：O(n)，n 为被修剪的字符数

(check (string-trim-right "  abc  ") => "  abc")
(check (string-trim-right "abc  ") => "abc")
(check (string-trim-right "  abc") => "  abc")
(check (string-trim-right "abc") => "abc")
(check (string-trim-right "") => "")
(check (string-trim-right "   ") => "")

;; 自定义谓词
(check (string-trim-right "xxabcxx" (lambda (c) (char=? c #\x))) => "xxabc")

;; 测试中文
(check (string-trim-right "  中文  ") => "  中文")


;; 测试使用整数索引作为 start/end
(check (string-trim-right "xxabcxx" (lambda (c) (char=? c #\x)) 2 5) => "abc")

;; 测试使用游标作为 start/end
(let* ((s "  abc  ")
       (start (string-cursor-start s))
       (end (string-cursor-end s)))
  (check (string-trim-right s char-whitespace? start end) => "  abc"))

;; 测试混合类型报错
(check-catch 'type-error (string-trim-right "abc" char-whitespace? 0 (string-cursor-end "abc")))

;; 测试 start > end 报错
(check-catch 'value-error (string-trim-right "abc" char-whitespace? 2 1))

;; 测试负数报错
(check-catch 'value-error (string-trim-right "abc" char-whitespace? -1 2))

(check-report)
