;; check-set-mode!
;; 设置测试运行与输出模式。
;;
;; 语法
;; ----
;; (check-set-mode! mode)
;;
;; 参数
;; ----
;; mode : symbol?
;; 测试输出模式，可选值为 'off, 'summary, 'report-failed 或 'report。
;;
;; 返回值
;; ----
;; unspecified
;; 切换内部运行模式。

(import (srfi srfi-78))

(check-set-mode! 'off)
(check (+ 1 1) => 2)

(check-set-mode! 'summary)
(check (+ 1 1) => 2)

(check-set-mode! 'report-failed)
(check (+ 1 1) => 2)

(check-set-mode! 'report)
(check (+ 1 1) => 2)

(check-set-mode! 'report-failed)

(check-report)
