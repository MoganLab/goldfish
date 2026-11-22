;; merge-adjacent-strings
;; 合并列表中相邻的字符串元素。
;;
;; 语法
;; ----
;; (merge-adjacent-strings items)
;;
;; 参数
;; ----
;; items : list
;; 混合字符串与符号表达式的列表。
;;
;; 返回值
;; ----
;; list
;; 返回合并相邻字符串后的列表。

(import (scheme base))
(import (liii check))
(import (liii cmark stem inline))

(check-set-mode! 'report-failed)

(check (merge-adjacent-strings '("a" "b" (code "x") "c" "d"))
  =>
  '("ab" (code "x") "cd")
) ;check

(check-report)
