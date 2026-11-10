;; terminal-type
;; 获取终端实例的类型标识符号。
;;
;; 语法
;; ----
;; (terminal-type term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; symbol?
;; 终端类型标识，如 'virtual、'standard。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui terminal protocol)
) ;import

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (check (terminal-type vt) => 'virtual)
) ;let

(check-report)
