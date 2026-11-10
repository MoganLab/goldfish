;; terminal-enter-raw-mode!
;; 让终端进入 raw 模式，关闭行缓冲与回显，便于 TUI 逐键读取输入。
;;
;; 语法
;; ----
;; (terminal-enter-raw-mode! term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; #t
;; 操作成功返回 #t。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (check (terminal-enter-raw-mode! vt) => #t)
) ;let

(check-catch 'type-error (terminal-enter-raw-mode! "not-a-terminal"))

(check-report)
