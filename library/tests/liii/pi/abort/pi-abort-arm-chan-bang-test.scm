(import (scheme base)
  (liii go)
  (liii check)
  (liii pi abort)
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
