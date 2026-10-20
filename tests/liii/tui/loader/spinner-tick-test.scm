(import (liii check)
        (liii tui loader spinner))

(check-set-mode! 'report-failed)

;; spinner-tick! 帧推进与重置测试

;; 1. line 帧推进与循环模运算
(let ((s (make-spinner "Working" :frames 'line)))
  (check (spinner-frame-index s) => 0)
  (check (spinner-current-frame s) => "-")
  ;; tick 1 -> "\"
  (check (spinner-tick! s) => "\\")
  (check (spinner-frame-index s) => 1)
  (check (spinner-current-frame s) => "\\")
  ;; tick 2 -> "|"
  (check (spinner-tick! s) => "|")
  (check (spinner-frame-index s) => 2)
  ;; tick 3 -> "/"
  (check (spinner-tick! s) => "/")
  (check (spinner-frame-index s) => 3)
  ;; tick 4 -> 循环回到 "-" (index 0)
  (check (spinner-tick! s) => "-")
  (check (spinner-frame-index s) => 0)
  (check (spinner-current-frame s) => "-"))

;; 2. dots 帧循环
(let ((s (make-spinner :frames 'dots)))
  (check (spinner-frame-index s) => 0)
  (let loop ((i 0))
    (when (< i 10)
      (spinner-tick! s)
      (loop (+ i 1))))
  ;; 转动 10 次后回到 index 0
  (check (spinner-frame-index s) => 0)
  (check (spinner-current-frame s) => "⠋"))

;; 3. reset! 重置回第 0 帧
(let ((s (make-spinner :frames 'pipe)))
  (spinner-tick! s)
  (spinner-tick! s)
  (spinner-tick! s)
  (check (spinner-frame-index s) => 3)
  (spinner-reset! s)
  (check (spinner-frame-index s) => 0)
  (check (spinner-current-frame s) => "┤"))

;; 4. on-change 回调触发
(let* ((notified 0)
       (s (make-spinner "Test"
                        :frames 'line
                        :on-change (lambda (sp) (set! notified (+ notified 1))))))
  (spinner-tick! s)
  (check notified => 1)
  (spinner-tick! s)
  (check notified => 2)
  (spinner-reset! s)
  (check notified => 3))

;; 5. 异常类型拦截
(check-catch 'type-error (spinner-tick! "not-a-spinner"))
(check-catch 'type-error (spinner-reset! 'bad))

(check-report)
