;; spinner-frames-resolve
;; 解析微调器帧规格，将预设符号或自定义字符串列表转换为帧字符串列表。
;;
;; 语法
;; ----
;; (spinner-frames-resolve spec)
;;
;; 参数
;; ----
;; spec : symbol? 或 (list of string?)
;; 预设符号（如 'dots, 'line, 'ascii）或由非空字符串构成的列表。
;;
;; 返回值
;; ----
;; list?
;; 对应样式的非空字符串帧列表。

(import (scheme base) (liii check) (liii tui loader presets))

(check-set-mode! 'report-failed)

;; 1. 预设符号解析
(check (spinner-frames-resolve 'line) => '("-" "\\" "|" "/"))
(check (spinner-frames-resolve 'ascii) => '("|" "/" "-" "\\"))
(check-true (pair? (spinner-frames-resolve 'dots)))

;; 2. 自定义字符串列表透传
(let ((custom '("a" "b" "c")))
  (check (spinner-frames-resolve custom) => custom)
) ;let

;; 3. 错误处理
(check-catch 'type-error (spinner-frames-resolve 'unknown-style))
(check-catch 'type-error (spinner-frames-resolve 123))
(check-catch 'type-error (spinner-frames-resolve '()))
(check-catch 'type-error (spinner-frames-resolve '(1 2 3)))

(check-report)
