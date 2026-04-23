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
;; 返回值
;; ------
;; integer?

(check (string-prefix-length "" "") => 0)
(check (string-prefix-length "abc" "abc") => 3)
(check (string-prefix-length "abc" "abd") => 2)
(check (string-prefix-length "abc" "xyz") => 0)
(check (string-prefix-length "prefix" "preface") => 4)
(check (string-prefix-length "中文" "中文测试") => 2)

;; string-suffix-length
(check (string-suffix-length "" "") => 0)
(check (string-suffix-length "abc" "abc") => 3)
(check (string-suffix-length "place" "preface") => 3)
(check (string-suffix-length "abc" "xyz") => 0)

;; string-prefix?
(check (string-prefix? "" "") => #t)
(check (string-prefix? "" "abc") => #t)
(check (string-prefix? "ab" "abc") => #t)
(check (string-prefix? "ac" "abc") => #f)
(check (string-prefix? "abc" "abc") => #t)
(check (string-prefix? "中文" "中文测试") => #t)

;; string-suffix?
(check (string-suffix? "" "") => #t)
(check (string-suffix? "" "abc") => #t)
(check (string-suffix? "bc" "abc") => #t)
(check (string-suffix? "ac" "abc") => #f)
(check (string-suffix? "abc" "abc") => #t)
(check (string-suffix? "测试" "中文测试") => #t)

(check-report)
