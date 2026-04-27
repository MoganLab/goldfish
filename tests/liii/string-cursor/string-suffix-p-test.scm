(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-suffix?
;; 判断 s1 是否是 s2 的后缀。
;;
;; 语法
;; ----
;; (string-suffix? s1 s2 [start1 end1 start2 end2])
;;
;; 参数
;; ----
;; s1, s2 : string
;; 要比较的两个字符串
;;
;; start1, end1, start2, end2 : integer 或 string-cursor? (可选)
;; 子串范围，默认为整个字符串
;;
;; 返回值
;; ------
;; boolean?
;; #t 如果 s1 是 s2 的后缀，否则 #f
;;
;; 说明
;; ----
;; 1. string-suffix? 是 SRFI-130 中的字符串比较函数
;; 2. 与 (liii string) 中的 string-suffix? 功能相同
;; 3. 性能：O(n)，n 为 s1 的长度

(check (string-suffix? "" "") => #t)
(check (string-suffix? "" "abc") => #t)
(check (string-suffix? "bc" "abc") => #t)
(check (string-suffix? "ac" "abc") => #f)
(check (string-suffix? "abc" "abc") => #t)
(check (string-suffix? "测试" "中文测试") => #t)


;; 测试使用游标作为 start/end
(let* ((s1 "bc")
       (s2 "abc")
       (start1 (string-cursor-start s1))
       (end1 (string-cursor-end s1))
       (start2 (string-cursor-start s2))
       (end2 (string-cursor-end s2)))
  (check (string-suffix? s1 s2 start1 end1 start2 end2) => #t))

;; 测试混合类型报错
(check-catch 'type-error (string-suffix? "abc" "abc" 0 (string-cursor-end "abc")))

;; 测试 start > end 报错
(check-catch 'value-error (string-suffix? "abc" "abc" 2 1))

;; 测试负数报错
(check-catch 'value-error (string-suffix? "abc" "abc" -1 2))

(check-report)
