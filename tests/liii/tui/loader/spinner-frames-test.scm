(import (liii check)
        (liii tui loader presets))

(check-set-mode! 'report-failed)

;; spinner-frames-resolve & presets
;; 预置转轮帧样式与状态图标解析

;; 1. 预设名称列表
(let ((p (spinner-presets)))
  (check (list? p) => #t)
  (check (member 'dots p) => '(dots line pipe star bounce pulse ascii))
  (check (not (not (member 'line p))) => #t)
  (check (not (not (member 'ascii p))) => #t))

;; 2. dots 帧解析
(let ((dots (spinner-frames-resolve 'dots)))
  (check (list? dots) => #t)
  (check (length dots) => 10)
  (check (car dots) => "⠋")
  (check (cadr dots) => "⠙"))

;; 3. line 帧解析
(let ((line (spinner-frames-resolve 'line)))
  (check line => '("-" "\\" "|" "/")))

;; 4. pipe 帧解析
(let ((pipe (spinner-frames-resolve 'pipe)))
  (check (length pipe) => 8)
  (check (car pipe) => "┤"))

;; 5. star 帧解析
(let ((star (spinner-frames-resolve 'star)))
  (check (length star) => 6)
  (check (car star) => "✶"))

;; 6. bounce 帧解析
(let ((bounce (spinner-frames-resolve 'bounce)))
  (check (length bounce) => 4)
  (check (car bounce) => "⠁"))

;; 7. pulse 帧解析
(let ((pulse (spinner-frames-resolve 'pulse)))
  (check (length pulse) => 6)
  (check (car pulse) => "█"))

;; 8. ascii 帧解析
(let ((asc (spinner-frames-resolve 'ascii)))
  (check asc => '("|" "/" "-" "\\")))

;; 9. 自定义帧列表解析
(let ((custom '("." "o" "O" "o")))
  (check (spinner-frames-resolve custom) => custom))
(check (spinner-frames-resolve '(".")) => '("."))
(check (spinner-frames-resolve '("")) => '(""))

;; 10. 异常情况拦截
(check-catch 'type-error (spinner-frames-resolve 'unknown-style))
(check-catch 'type-error (spinner-frames-resolve '()))
(check-catch 'type-error (spinner-frames-resolve 123))
(check-catch 'type-error (spinner-frames-resolve '(1 "b" "c")))
(check-catch 'type-error (spinner-frames-resolve '("a" 2 "c")))
(check-catch 'type-error (spinner-frames-resolve '("a" "b" 3)))

;; 11. 状态图标解析
(check (spinner-status-icon 'spinning) => #f)
(check (spinner-status-icon 'success) => "✔")
(check (spinner-status-icon 'error) => "✖")
(check (spinner-status-icon 'warning) => "⚠")
(check (spinner-status-icon 'stopped) => "■")
(check-catch 'type-error (spinner-status-icon 'invalid-status))

(check-report)
