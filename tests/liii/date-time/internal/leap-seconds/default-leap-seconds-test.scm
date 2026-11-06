;; default-leap-seconds
;; 内置的 IERS 闰秒表，按 UNIX 时间升序排列。
;;
;; 语法
;; ----
;; default-leap-seconds
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; vector? 元素为 (unix-second . tai-offset) 序对的向量。

(import (liii check) (liii date-time internal leap-seconds))

(check-set-mode! 'report-failed)

(check-true (vector? default-leap-seconds))
(check (vector-length default-leap-seconds) => 28)
(check (vector-ref default-leap-seconds 0) => '(63072000 . 10))

(check-report)
