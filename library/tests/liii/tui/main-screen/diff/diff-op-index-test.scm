;; diff-op-index
;; 获取差分操作作用的行索引。
;;
;; 语法
;; ----
;; (diff-op-index op)
;;
;; 参数
;; ----
;; op : list?
;; 差分操作，形如 (keep index)、(update index text)、(append index text) 或 (clear index)。
;;
;; 返回值
;; ----
;; integer?
;; 操作作用的行索引。

(import (scheme base) (liii check) (liii tui main-screen diff))

(check-set-mode! 'report-failed)

(check (diff-op-index '(keep 0)) => 0)
(check (diff-op-index '(update 1 "foo")) => 1)
(check (diff-op-index '(append 2 "bar")) => 2)
(check (diff-op-index '(clear 3)) => 3)

(check-report)
