;; terminal-paste-mode?
;; 查询终端的粘贴（bracketed paste）模式是否已开启。
;;
;; 语法
;; ----
;; (terminal-paste-mode?)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; boolean?
;; 粘贴模式开启返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui terminal poll))

(check-set-mode! 'report-failed)

;; 粘贴模式默认关闭
(check (terminal-paste-mode?) => #f)

(check-report)
