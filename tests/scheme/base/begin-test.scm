(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; begin
;; 按顺序执行多个表达式，返回最后一个的值。
;;
;; 语法
;; ----
;; (begin expression ...)
;;
;; 参数
;; ----
;; expression ... : 任意类型
;; 要顺序执行的表达式。
;;
;; 返回值
;; ------
;; 任意类型
;; 最后一个表达式的值。
;;
;; 说明
;; ----
;; 1. 表达式按顺序求值
;; 2. 常用于需要多个操作的上下文
;; 3. 隐式 begin 存在于 lambda、let 等结构中
(check (begin 1 2 3) => 3)
(check (begin (+ 1 2) (* 3 4)) => 12)
(let ((x 0))
  (begin
    (set! x 1)
    (set! x 2)
    (set! x 3)
  ) ;begin
  (check x => 3)
) ;let

(check-report)
