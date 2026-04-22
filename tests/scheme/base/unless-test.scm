(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; unless
;; 当条件为假时执行表达式。
;;
;; 语法
;; ----
;; (unless test expr ...)
;;
;; 参数
;; ----
;; test : 任意类型
;; 条件表达式。
;; expr ... : 任意类型
;; 条件为假时执行的表达式。
;;
;; 返回值
;; ------
;; 任意类型
;; 最后一个 expr 的值，或如果条件为真则返回未指定值。
;;
;; 说明
;; ----
;; 1. 是 when 的反向逻辑
;; 2. 条件为真时不执行任何操作
(check (unless #f 'no) => 'no)
(check (unless #t 'yes) => #<unspecified>)
(let ((x 0))
  (unless (> x 0) (set! x 1))
  (check x => 1)
) ;let
(let ((x 0))
  (unless (< x 0) (set! x 2))
  (check x => 2)
) ;let

(check-report)
