;; pi-abort-arm-chan!
;; 绑定中止信号通道，并排空通道中的残留信号。
;;
;; 语法
;; ----
;; (pi-abort-arm-chan! chan)
;;
;; 参数
;; ----
;; chan : channel
;; 用于接收 abort 信号的通道。
;;
;; 返回值
;; ----
;; 未指定
;; 绑定新的通道并复位中止状态。

(import (scheme base)
  (liii go)
  (liii check)
  (liii pi ai abort)
) ;import

(check-set-mode! 'report-failed)

;; 重新 arm 时排空 chan 中残留信号
(let ((ch (make-chan 1)))
  (chan-send! ch 'abort 0)
  (pi-abort-arm-chan! ch)
  (check (pi-aborted?) => #f))

;; arm 排空多个残留信号
(let ((ch (make-chan 4)))
  (chan-send! ch 'abort 0)
  (chan-send! ch 'abort 0)
  (chan-send! ch 'abort 0)
  (pi-abort-arm-chan! ch)
  (check (pi-aborted?) => #f))

(check-report)
