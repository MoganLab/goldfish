(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-contains
;; 在 s1 中查找 s2 第一次出现的位置，返回 cursor。
;; 如果没有找到，返回 #f。
;;
;; 语法
;; ----
;; (string-contains s1 s2 [start1 end1 start2 end2])
;;
;; 返回值
;; ------
;; string-cursor? 或 #f

;; 基本测试
(let ((result (string-contains "abcdef" "cd")))
  (check (string-cursor? result) => #t)
  (check (string-cursor->index "abcdef" result) => 2))

(check (string-contains "abcdef" "xyz") => #f)
(check (string-cursor->index "abcdef" (string-contains "abcdef" "")) => 0)
(check (string-cursor->index "" (string-contains "" "")) => 0)
(check (string-contains "" "abc") => #f)

;; 测试中文
(let ((result (string-contains "我是中国人" "中国")))
  (check (string-cursor->index "我是中国人" result) => 2))

;; 测试多次出现
(let ((result (string-contains "ababab" "ab")))
  (check (string-cursor->index "ababab" result) => 0))

;; string-contains-right
;; 在 s1 中查找 s2 最后一次出现的位置，返回 cursor。
(let ((result (string-contains-right "ababab" "ab")))
  (check (string-cursor->index "ababab" result) => 4))

(check (string-contains-right "abcdef" "xyz") => #f)
(check (string-cursor->index "abcdef" (string-contains-right "abcdef" "")) => 6)

;; 测试中文
(let ((result (string-contains-right "中国中国" "中国")))
  (check (string-cursor->index "中国中国" result) => 2))

(check-report)
