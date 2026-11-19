;; spinner-status-icon
;; 根据微调器终态符号获取对应的状态指示图标字符。
;;
;; 语法
;; ----
;; (spinner-status-icon status)
;;
;; 参数
;; ----
;; status : symbol?
;; 状态符号：'spinning, 'success, 'error, 'warning, 'stopped。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 若为 'spinning 返回 #f（使用动态旋转帧），其余终态返回对应图标字符。

(import (scheme base) (liii check) (liii tui loader presets))

(check-set-mode! 'report-failed)

(check (spinner-status-icon 'spinning) => #f)
(check (spinner-status-icon 'success) => "✔")
(check (spinner-status-icon 'error) => "✖")
(check (spinner-status-icon 'warning) => "⚠")
(check (spinner-status-icon 'stopped) => "■")

(check-catch 'type-error (spinner-status-icon 'unknown))
(check-catch 'type-error (spinner-status-icon 123))

(check-report)
