;; count-status
;; 统计结果列表中处于指定状态的结果数量。
;;
;; 语法
;; ----
;; (count-status sym results)
;;
;; 参数
;; ----
;; sym : symbol?
;; 待统计的状态符号。
;; results : list
;; 结果三元表列表，每项形如 (路径 状态 失败信息)。
;;
;; 返回值
;; ------
;; number?
;; 处于该状态的结果数量。

(import (scheme base) (liii check) (liii goldtool parallel))

(check-set-mode! 'report-failed)

(define results
  '(("a.scm" ok "done")
    ("b.scm" failed "boom")
    ("c.scm" ok "done")
    ("d.scm" failed "boom")
  )
) ;define

;; 统计指定状态的数量
(check (count-status 'ok results) => 2)
(check (count-status 'failed results) => 2)

;; 不存在的状态统计结果为 0
(check (count-status 'unknown results) => 0)

;; 空结果列表统计结果为 0
(check (count-status 'ok '()) => 0)

(check-report)
