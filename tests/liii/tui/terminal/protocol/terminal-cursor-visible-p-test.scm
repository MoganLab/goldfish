;; terminal-cursor-visible?
;; 查询终端光标当前是否可见。
;;
;; 语法
;; ----
;; (terminal-cursor-visible? term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; boolean?
;; 光标可见返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui terminal))

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (check (terminal-cursor-visible? vt) => #t)
  (terminal-hide-cursor! vt)
  (check (terminal-cursor-visible? vt) => #f)
  (terminal-show-cursor! vt)
  (check (terminal-cursor-visible? vt) => #t)
) ;let

(check-catch 'type-error (terminal-cursor-visible? "not-a-terminal"))

(check-report)
