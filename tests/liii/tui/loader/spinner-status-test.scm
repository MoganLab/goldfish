(import (liii check)
        (liii tui loader spinner))

(check-set-mode! 'report-failed)

;; spinner-status 状态转换与完成图标测试

;; 1. 默认 spinning 状态
(let ((s (make-spinner "Processing" :frames 'line)))
  (check (spinner-status s) => 'spinning)
  (check (spinner-current-frame s) => "-"))

;; 2. success 状态切换
(let ((s (make-spinner "Build" :frames 'line)))
  (spinner-success! s "Build passed")
  (check (spinner-status s) => 'success)
  (check (spinner-text s) => "Build passed")
  (check (spinner-current-frame s) => "✔")
  ;; tick! 不再推进动画帧，依然返回 "✔"
  (check (spinner-tick! s) => "✔")
  (check (spinner-status s) => 'success))

;; 3. fail / error 状态切换
(let ((s (make-spinner "Test" :frames 'line)))
  (spinner-fail! s "Test failed: timeout")
  (check (spinner-status s) => 'error)
  (check (spinner-text s) => "Test failed: timeout")
  (check (spinner-current-frame s) => "✖"))

(let ((s (make-spinner "Compile")))
  (spinner-error! s "Syntax error")
  (check (spinner-status s) => 'error)
  (check (spinner-text s) => "Syntax error")
  (check (spinner-current-frame s) => "✖"))

;; 4. warn 状态切换
(let ((s (make-spinner "Lint")))
  (spinner-warn! s "Lint warnings detected")
  (check (spinner-status s) => 'warning)
  (check (spinner-text s) => "Lint warnings detected")
  (check (spinner-current-frame s) => "⚠"))

;; 5. stop 状态切换
(let ((s (make-spinner "Download" :frames 'dots)))
  (spinner-stop! s)
  (check (spinner-status s) => 'stopped)
  (check (spinner-current-frame s) => "■"))

;; 6. reset! 能够从已完成状态重置回 spinning
(let ((s (make-spinner "Run" :frames 'line)))
  (spinner-success! s "Done")
  (check (spinner-status s) => 'success)
  (spinner-reset! s)
  (check (spinner-status s) => 'spinning)
  (check (spinner-current-frame s) => "-"))

(check-report)
