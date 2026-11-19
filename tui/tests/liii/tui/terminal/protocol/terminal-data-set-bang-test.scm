;; terminal-data-set!
;; 设置终端实例所持有的私有状态或负载数据。
;;
;; 语法
;; ----
;; (terminal-data-set! term val)
;;
;; 参数
;; ----
;; term : terminal?
;; 终端实例。
;;
;; val : any
;; 要存入的新数据。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base)
  (liii check)
  (liii tui terminal)
  (liii tui terminal protocol)
) ;import

(check-set-mode! 'report-failed)

(let ((vt (make-virtual-terminal :width 20 :height 5)))
  (terminal-data-set! vt 'custom)
  (check (terminal-data vt) => 'custom)
) ;let

(check-report)
