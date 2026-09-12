(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor=?
;; 比较两个游标是否指向同一位置。
;;
;; 语法
;; ----
;; (string-cursor=? cursor1 cursor2)
;;
;; 参数
;; ----
;; cursor1, cursor2 : string-cursor?
;; 要比较的两个游标
;;
;; 返回值
;; ------
;; boolean?
;; 相等返回 #t，否则返回 #f
;;
;; 说明
;; ----
;; 1. string-cursor=? 是 SRFI-130 中的游标比较函数
;; 2. 性能：O(1)，直接比较索引
;; 3. 支持 Unicode 字符串游标的比较
;;
;; 相关实现
;; --------
;; (liii string-cursor) 独有函数，无 (liii string) 对应版本
;; 参见: gf doc liii/string-cursor "string-cursor=?"
(let* ((s "abcde") (c0 (string-index->cursor s 0)) (c1 (string-index->cursor s 1)))
  (check (string-cursor=? c0 c0) => #t)
  (check (string-cursor=? c0 c1) => #f)
  (check (string-cursor=? c1 c0) => #f)
) ;let*

(check-report)
