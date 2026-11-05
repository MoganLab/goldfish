;; pi-abort-reset!
;; 解除中止信号通道绑定并清除已缓存的中止状态。
;;
;; 语法
;; ----
;; (pi-abort-reset!)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; 未指定
;; 重置后 pi-aborted? 在未收到新信号时返回 #f。

(import (scheme base)
  (liii go)
  (liii check)
  (liii pi ai abort)
) ;import

(check-set-mode! 'report-failed)

;; reset 后回到未 arm 的安全默认状态
(let ((ch (make-chan 1)))
  (pi-abort-arm-chan! ch)
  (chan-send! ch 'abort 0)
  (check (pi-aborted?) => #t)
  (pi-abort-reset!)
  (check (pi-aborted?) => #f))

(check-report)
