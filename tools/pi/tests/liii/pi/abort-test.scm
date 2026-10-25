(import (scheme base)
  (liii go)
  (liii check)
  (liii pi abort)
) ;import

(check-set-mode! 'report-failed)

;; 1. 未 arm 时调用 pi-aborted? 安全返回 #f
(pi-abort-reset!)
(check (pi-aborted?) => #f)

;; 2. arm chan 后无信号：返回 #f
(let ((ch (make-chan 1)))
  (pi-abort-arm-chan! ch)
  (check (pi-aborted?) => #f)
) ;let

;; 3. chan 中收到 abort 信号后 pi-aborted? 为 #t（worker 检查点场景）
(let ((ch (make-chan 1)))
  (pi-abort-arm-chan! ch)
  (chan-send! ch 'abort 0)
  (check (pi-aborted?) => #t)
) ;let

;; 4. 一次性语义：首次探测到中止后置位缓存，chan 中信号已被消费，
;;    后续调用不会因信号丢失而反复探测，仍保持 #t
(let ((ch (make-chan 1)))
  (pi-abort-arm-chan! ch)
  (chan-send! ch 'abort 0)
  (check (pi-aborted?) => #t)
  (check (pi-aborted?) => #t)
) ;let

;; 5. 重新 arm 时排空 chan 中残留信号：上一轮任务的残留 abort
;;    不影响新一轮任务
(let ((ch (make-chan 1)))
  (chan-send! ch 'abort 0)
  (pi-abort-arm-chan! ch)
  (check (pi-aborted?) => #f)
) ;let

;; 6. arm 排空多个残留信号
(let ((ch (make-chan 4)))
  (chan-send! ch 'abort 0)
  (chan-send! ch 'abort 0)
  (chan-send! ch 'abort 0)
  (pi-abort-arm-chan! ch)
  (check (pi-aborted?) => #f)
) ;let

;; 7. reset 后回到安全默认：未 arm 状态
(let ((ch (make-chan 1)))
  (pi-abort-arm-chan! ch)
  (chan-send! ch 'abort 0)
  (check (pi-aborted?) => #t)
  (pi-abort-reset!)
  (check (pi-aborted?) => #f)
) ;let

(check-report)
