;; make-history
;; 创建输入历史管理器实例。
;;
;; 语法
;; ----
;; (make-history [max-size])
;;
;; 参数
;; ----
;; max-size : integer? (可选，默认为 100)
;; 最大保留的历史条目数量。
;;
;; 返回值
;; ----
;; history?
;; 历史管理器实例。

(import (scheme base) (liii check) (liii tui editor history))

(check-set-mode! 'report-failed)

(let ((h (make-history 50)))
  (check-true (history? h))
  (check (history-max-size h) => 50)
  (check (history-entries h) => '())
  (check (history-index h) => #f)
  (check (history-draft h) => #f)
) ;let

(check-report)
