;; terminal-show-cursor!
;; 显示终端光标。
;;
;; 语法
;; ----
;; (terminal-show-cursor! term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (terminal-hide-cursor! vt)
  (check (terminal-cursor-visible? vt) => #f)
  (terminal-show-cursor! vt)
  (check (terminal-cursor-visible? vt) => #t)
) ;let

(check-catch 'type-error (terminal-show-cursor! "not-a-terminal"))

(check-report)
