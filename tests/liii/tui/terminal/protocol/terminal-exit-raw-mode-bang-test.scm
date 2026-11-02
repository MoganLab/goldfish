;; terminal-exit-raw-mode!
;; 让终端退出 raw 模式，恢复行缓冲与回显。
;;
;; 语法
;; ----
;; (terminal-exit-raw-mode! term)
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
  (check (terminal-exit-raw-mode! vt) => #t)
) ;let

(check-catch 'type-error (terminal-exit-raw-mode! "not-a-terminal"))

(check-report)
