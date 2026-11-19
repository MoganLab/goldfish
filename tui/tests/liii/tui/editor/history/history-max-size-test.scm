;; history-max-size
;; 获取历史管理器允许保存的最大容量限制。
;;
;; 语法
;; ----
;; (history-max-size h)
;;
;; 参数
;; ----
;; h : history?
;; 历史管理器实例。
;;
;; 返回值
;; ----
;; integer?
;; 最大历史条目容量。

(import (scheme base) (liii check) (liii tui editor history))

(check-set-mode! 'report-failed)

(let ((h (make-history 20)))
  (check (history-max-size h) => 20)
) ;let

(check-report)
