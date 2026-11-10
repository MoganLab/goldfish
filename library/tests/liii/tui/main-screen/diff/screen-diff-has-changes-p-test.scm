;; screen-diff-has-changes?
;; 判断差分操作序列中是否存在实际内容变化。
;;
;; 语法
;; ----
;; (screen-diff-has-changes? diff-ops)
;;
;; 参数
;; ----
;; diff-ops : list?
;; 差分操作列表。
;;
;; 返回值
;; ----
;; boolean?
;; 若存在 update/append/clear 操作返回 #t，仅有 keep 操作返回 #f。

(import (scheme base) (liii check) (liii tui main-screen diff))

(check-set-mode! 'report-failed)

(check (screen-diff-has-changes? '()) => #f)
(check (screen-diff-has-changes? '((keep 0) (keep 1))) => #f)
(check (screen-diff-has-changes? '((keep 0) (update 1 "x"))) => #t)
(check (screen-diff-has-changes? '((keep 0) (append 1 "x"))) => #t)
(check (screen-diff-has-changes? '((clear 0))) => #t)

(check-report)
