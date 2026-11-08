;; stream-range
;; 生成 [first, past) 方向按步长的有限算术流。
;;
;; 语法
;; ----
;; (stream-range first past [step])
;;
;; 参数
;; ----
;; first : number?
;; 首元素。
;;
;; past : number?
;; 终界（不含）。
;;
;; step : number?
;; 步长；缺省按方向取 1 或 -1。
;; 返回值
;; ----
;; stream?
;; 算术序列流；方向由 first 与 past 或 step 符号决定。
;;
;; 说明
;; ----
;; 步长为 0 且 first≠past 时不终止。

;;
;; 错误处理
;; ----
;; type-error
;; first / past / step 任一不是数值

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-range "four" 0))
(check-catch 'type-error (stream-range 0 "four"))
(check-catch 'type-error (stream-range 1 2 "three"))
(check (stream->list (stream-range 0 5)) => '(0 1 2 3 4))
(check (stream->list (stream-range 5 0)) => '(5 4 3 2 1))
(check (stream->list (stream-range 0 5 2)) => '(0 2 4))
(check (stream->list (stream-range 5 0 -2)) => '(5 3 1))
(check (stream->list (stream-range 0 1 -1)) => '())

(check-report)
