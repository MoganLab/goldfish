(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-suffix-length
;; 返回两个字符串的最长公共后缀长度。
;;
;; 语法
;; ----
;; (string-suffix-length s1 s2 [start1 end1 start2 end2])
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
;; integer?
;; 最长公共后缀的字符数
;;
;; 说明
;; ----
;; 1. string-suffix-length 是 SRFI-130 中的字符串比较函数
;; 2. 与 (liii string) 中的 string-suffix-length 功能相同
;; 3. 性能：O(n)，n 为公共后缀长度

(check (string-suffix-length "" "") => 0)
(check (string-suffix-length "abc" "abc") => 3)
(check (string-suffix-length "place" "preface") => 3)
(check (string-suffix-length "abc" "xyz") => 0)


;; 测试使用整数索引作为 start/end
(check (string-suffix-length "place" "preface" 0 5 0 7) => 3)
(check (string-suffix-length "place" "preface" 2 5 0 7) => 3)

;; 测试使用游标作为 start/end
(let* ((s1 "place")
       (s2 "preface")
       (start1 (string-cursor-start s1))
       (end1 (string-cursor-end s1))
       (start2 (string-cursor-start s2))
       (end2 (string-cursor-end s2)))
  (check (string-suffix-length s1 s2 start1 end1 start2 end2) => 3))

;; 测试混合类型报错
(check-catch 'type-error (string-suffix-length "abc" "abc" 0 (string-cursor-end "abc")))

;; 测试 start > end 报错
(check-catch 'value-error (string-suffix-length "abc" "abc" 2 1))

;; 测试负数报错
(check-catch 'value-error (string-suffix-length "abc" "abc" -1 2))

;; 测试 end 超出字符串长度不崩溃（应被截断）
(check (string-suffix-length "abc" "abc" 0 100 0 100) => 3)

(check-report)
