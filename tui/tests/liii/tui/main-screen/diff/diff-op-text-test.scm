;; diff-op-text
;; 获取差分操作携带的文本内容。
;;
;; 语法
;; ----
;; (diff-op-text op)
;;
;; 参数
;; ----
;; op : list?
;; 差分操作，形如 (keep index)、(update index text)、(append index text) 或 (clear index)。
;;
;; 返回值
;; ----
;; string? 或 #f
;; update/append 操作返回文本内容，keep/clear 操作返回 #f。

(import (scheme base) (liii check) (liii tui main-screen diff))

(check-set-mode! 'report-failed)

(check (diff-op-text '(keep 0)) => #f)
(check (diff-op-text '(update 1 "foo")) => "foo")
(check (diff-op-text '(append 2 "bar")) => "bar")
(check (diff-op-text '(clear 3)) => #f)

(check-report)
