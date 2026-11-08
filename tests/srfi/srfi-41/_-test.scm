;; _
;; stream-match 的通配符模式（哑绑定导出）。
;;
;; 语法
;; ----
;; _（stream-match 模式内的字面量）
;;
;; 参数
;; ----
;; 无（宏字面量标识符）
;; 返回值
;; ----
;; 任何
;; 不会作为过程被调用。
;;
;; 说明
;; ----
;; SRFI 41 要求导出 _ 以便 stream-match 模式中引用统一标识符；
;; Goldfish 实现里匹配由 syntax-rules 字面量完成，此导出仅为规范兼容。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

;; 通配符行为由 stream-match 测试覆盖：
(check (stream-match (stream 1) ((_ ) 'ok)) => 'ok)
(check (stream-match (stream 1 2 3) ((a . _) a)) => 1)

(check-report)
