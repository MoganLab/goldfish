(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor?
;; 判断一个值是否为字符串游标。
;;
;; 语法
;; ----
;; (string-cursor? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要判断的值
;;
;; 返回值
;; ------
;; boolean?
;; #t 如果 obj 是字符串游标，否则 #f
;;
;; 说明
;; ----
;; 1. string-cursor? 是 SRFI-130 中的游标谓词函数
;; 2. 与 (liii string) 的区别：(liii string-cursor) 提供了完整的游标操作支持
;; 3. 性能：O(1)

;; 基本测试
(let ((s "abc") (c (string-cursor-start "abc")))
  (check (string-cursor? c) => #t)
  (check (string-cursor? 'not-a-cursor) => #f)
  (check (string-cursor? 0) => #f)
  (check (string-cursor? "abc") => #f)
  (check (string-cursor? '()) => #f)
) ;let

(check-report)
