;; diff-op-type
;; 获取差分操作的类型。
;;
;; 语法
;; ----
;; (diff-op-type op)
;;
;; 参数
;; ----
;; op : list?
;; 差分操作，形如 (keep index)、(update index text)、(append index text) 或 (clear index)。
;;
;; 返回值
;; ----
;; symbol?
;; 操作类型，'keep、'update、'append 或 'clear。

(import (scheme base) (liii check) (liii tui main-screen diff))

(check-set-mode! 'report-failed)

(check (diff-op-type '(keep 0)) => 'keep)
(check (diff-op-type '(update 1 "foo")) => 'update)
(check (diff-op-type '(append 2 "bar")) => 'append)
(check (diff-op-type '(clear 3)) => 'clear)

(check-report)
