;; terminal-clear-line
;; 清空终端当前光标所在行的内容。
;;
;; 语法
;; ----
;; (terminal-clear-line term)
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
  (terminal-set-cursor! vt 0 2)
  (terminal-write vt "row")
  (terminal-clear-line vt)
  (check (list-ref (virtual-terminal-screen vt) 2) => (make-string 20 #\space))
  ;; 其他行不受影响
  (check (list-ref (virtual-terminal-screen vt) 0) => (make-string 20 #\space))
) ;let

(check-catch 'type-error (terminal-clear-line "not-a-terminal"))

(check-report)
