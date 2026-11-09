;; stream-from
;; 从 first 开始按步长生成无限算术流。
;;
;; 语法
;; ----
;; (stream-from first [step])
;;
;; 参数
;; ----
;; first : number?
;; 首元素。
;; step : number?
;; 步长，缺省 1，可为负。
;;
;; 返回值
;; ----
;; stream?
;; 算术级数无限流。
;;
;; 说明
;; ----
;;
;; 错误处理
;; ----
;; type-error
;; first 或 step 不是数值

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-from "four"))
(check-catch 'type-error (stream-from 1 "four"))
(check (stream-ref (stream-from 0) 100) => 100)
(check (stream-ref (stream-from 1 2) 100) => 201)
(check (stream-ref (stream-from 0 -1) 100) => -100)
(check (stream->list 3 (stream-from 5)) => '(5 6 7))

(check-report)
