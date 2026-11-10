;; pi-aborted?
;; 非阻塞检查当前会话是否已收到中止信号。
;;
;; 语法
;; ----
;; (pi-aborted?)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; boolean
;; 已收到中止信号时返回 #t，否则返回 #f。

(import (scheme base)
  (liii go)
  (liii check)
  (liii pi ai abort)
) ;import

(check-set-mode! 'report-failed)

;; 未 arm 时安全返回 #f
(pi-abort-reset!)
(check (pi-aborted?) => #f)

;; arm chan 后无信号返回 #f
(let ((ch (make-chan 1)))
  (pi-abort-arm-chan! ch)
  (check (pi-aborted?) => #f))

;; chan 中收到 abort 信号后返回 #t
(let ((ch (make-chan 1)))
  (pi-abort-arm-chan! ch)
  (chan-send! ch 'abort 0)
  (check (pi-aborted?) => #t))

;; 一次性语义：置位缓存后仍保持 #t
(let ((ch (make-chan 1)))
  (pi-abort-arm-chan! ch)
  (chan-send! ch 'abort 0)
  (check (pi-aborted?) => #t)
  (check (pi-aborted?) => #t))

(check-report)
