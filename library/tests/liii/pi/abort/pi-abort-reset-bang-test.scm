(import (scheme base)
  (liii go)
  (liii check)
  (liii pi abort)
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
