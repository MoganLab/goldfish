;; terminal-set-paste-mode!
;; 开启或关闭终端的粘贴（bracketed paste）模式。
;;
;; 语法
;; ----
;; (terminal-set-paste-mode! enabled?)
;;
;; 参数
;; ----
;; enabled? : boolean?
;; #t 开启粘贴模式，#f 关闭。
;;
;; 返回值
;; ----
;; boolean?
;; 设置后的粘贴模式状态。

(import (scheme base) (liii check) (liii tui terminal poll))

(check-set-mode! 'report-failed)

(check (terminal-set-paste-mode! #t) => #t)
(check (terminal-paste-mode?) => #t)
(check (terminal-set-paste-mode! #f) => #f)
(check (terminal-paste-mode?) => #f)

(check-report)
