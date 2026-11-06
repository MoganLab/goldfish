;; terminal-data
;; 获取终端实例所持有的私有状态或负载数据。
;;
;; 语法
;; ----
;; (terminal-data term)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; 返回值
;; ----
;; any
;; 终端持有的私有数据。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui terminal protocol)
) ;import

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (check-true (not (not (terminal-data vt))))
) ;let

(check-report)
