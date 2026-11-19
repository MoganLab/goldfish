;; terminal-hide-cursor!
;; 隐藏终端光标。
;;
;; 语法
;; ----
;; (terminal-hide-cursor! term)
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
  (check (terminal-cursor-visible? vt) => #t)
  (terminal-hide-cursor! vt)
  (check (terminal-cursor-visible? vt) => #f)
) ;let

(check-catch 'type-error (terminal-hide-cursor! "not-a-terminal"))

(check-report)
