;; spinner-presets
;; 获取所有内置的微调器（spinner）预设样式符号列表。
;;
;; 语法
;; ----
;; (spinner-presets)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; list?
;; 包含 '(dots line pipe star bounce pulse ascii) 等符号的列表。

(import (scheme base) (liii check) (liii tui loader presets))

(check-set-mode! 'report-failed)

(let ((p (spinner-presets)))
  (check-true (list? p))
  (check (member 'dots p eq?) => '(dots line pipe star bounce pulse ascii))
  (check-true (not (not (memq 'line p))))
  (check-true (not (not (memq 'ascii p))))
) ;let

(check-report)
