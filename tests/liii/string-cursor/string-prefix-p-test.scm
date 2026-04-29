(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-prefix?
;; 判断 s1 是否是 s2 的前缀。
;;
;; 语法
;; ----
;; (string-prefix? s1 s2 [start1 end1 start2 end2])
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
;; #t 如果 s1 是 s2 的前缀，否则 #f
;;
;; 说明
;; ----
;; 1. string-prefix? 是 SRFI-130 中的字符串比较函数
;; 2. 与 (liii string) 中的 string-prefix? 功能相同
;; 3. 性能：O(n)，n 为 s1 的长度

(check (string-prefix? "" "") => #t)
(check (string-prefix? "" "abc") => #t)
(check (string-prefix? "ab" "abc") => #t)
(check (string-prefix? "ac" "abc") => #f)
(check (string-prefix? "abc" "abc") => #t)
(check (string-prefix? "中文" "中文测试") => #t)


;; 测试使用游标作为 start/end
(let* ((s1 "ab")
       (s2 "abc")
       (start1 (string-cursor-start s1))
       (end1 (string-cursor-end s1))
       (start2 (string-cursor-start s2))
       (end2 (string-cursor-end s2))
      ) ;
  (check (string-prefix? s1 s2 start1 end1 start2 end2) => #t)
) ;let*

;; 测试混合类型报错
(check-catch 'type-error
  (string-prefix? "abc" "abc" 0 (string-cursor-end "abc"))
) ;check-catch

;; 测试 start > end 报错
(check-catch 'value-error (string-prefix? "abc" "abc" 2 1))

;; 测试负数报错
(check-catch 'value-error (string-prefix? "abc" "abc" -1 2))

(check-report)
