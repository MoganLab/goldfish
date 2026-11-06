;; string-cursor>=?
;; 比较两个字符串游标的位置。
;;
;; 语法
;; ----
;; (string-cursor>=? cursor1 cursor2)
;;
;; 参数
;; ----
;; cursor1 : string-cursor? 或 integer?
;; 第一个游标，也可以直接给出整数索引。
;;
;; cursor2 : string-cursor? 或 integer?
;; 第二个游标，也可以直接给出整数索引。
;;
;; 返回值
;; ----
;; boolean
;; cursor1 的位置不小于 cursor2 时返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 性能：O(1)，直接比较索引。
;;
;; 错误处理
;; ----
;; type-error 当参数既不是游标也不是整数索引时抛出。

(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

(let* ((s "abcde") (c0 (string-index->cursor s 0)) (c1 (string-index->cursor s 1)))
  (check (string-cursor>=? c1 c0) => #t)
  (check (string-cursor>=? c1 c1) => #t)
  (check (string-cursor>=? c0 c1) => #f)
) ;let*

(check-report)
