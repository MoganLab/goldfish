;; history-push!
;; 向历史管理器头部追加一条新记录，自动去重与容量修剪，并重置浏览状态。
;;
;; 语法
;; ----
;; (history-push! h text)
;;
;; 参数
;; ----
;; h : history?
;; 历史管理器实例。
;;
;; text : string?
;; 待追加的记录文本（空串忽略）。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor history))

(check-set-mode! 'report-failed)

(let ((h (make-history 2)))
  (history-push! h "cmd1")
  (history-push! h "cmd2")
  ;; 连续重复不追加
  (history-push! h "cmd2")
  (check (history-entries h) => '("cmd2" "cmd1"))
  ;; 超出容量剔除最旧的
  (history-push! h "cmd3")
  (check (history-entries h) => '("cmd3" "cmd2"))
) ;let

(check-report)
