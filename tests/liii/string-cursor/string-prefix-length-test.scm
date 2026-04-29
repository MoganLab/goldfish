(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-prefix-length
;; 返回两个字符串的最长公共前缀长度。
;;
;; 语法
;; ----
;; (string-prefix-length s1 s2 [start1 end1 start2 end2])
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
;; 最长公共前缀的字符数
;;
;; 说明
;; ----
;; 1. string-prefix-length 是 SRFI-130 中的字符串比较函数
;; 2. 与 (liii string) 中的 string-prefix-length 功能相同
;; 3. 性能：O(n)，n 为公共前缀长度

(check (string-prefix-length "" "") => 0)
(check (string-prefix-length "abc" "abc") => 3)
(check (string-prefix-length "abc" "abd") => 2)
(check (string-prefix-length "abc" "xyz") => 0)
(check (string-prefix-length "prefix" "preface") => 4)
(check (string-prefix-length "中文" "中文测试") => 2)


;; 测试使用整数索引作为 start/end
(check (string-prefix-length "prefix" "preface" 0 6 0 7) => 4)
(check (string-prefix-length "prefix" "preface" 0 3 0 7) => 3)

;; 测试使用游标作为 start/end
(let* ((s1 "prefix")
       (s2 "preface")
       (start1 (string-cursor-start s1))
       (end1 (string-cursor-end s1))
       (start2 (string-cursor-start s2))
       (end2 (string-cursor-end s2))
      ) ;
  (check (string-prefix-length s1 s2 start1 end1 start2 end2) => 4)
) ;let*

;; 测试混合类型报错
(check-catch 'type-error
  (string-prefix-length "abc" "abc" 0 (string-cursor-end "abc"))
) ;check-catch

;; 测试 start > end 报错
(check-catch 'value-error (string-prefix-length "abc" "abc" 2 1))

;; 测试负数报错
(check-catch 'value-error (string-prefix-length "abc" "abc" -1 2))

;; 测试 end 超出字符串长度不崩溃（应被截断）
(check (string-prefix-length "abc" "abc" 0 100 0 100) => 3)

(check-report)
