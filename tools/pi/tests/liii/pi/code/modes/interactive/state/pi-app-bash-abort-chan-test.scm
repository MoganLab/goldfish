;; pi-app-bash-abort-chan
;; 获取应用实例中用于中止 ! 命令的 ESC 中止信号通道。
;;
;; 语法
;; ----
;; (pi-app-bash-abort-chan app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; any
;; ! 命令的 ESC 中止信号通道对象（容量 1，与 agent 的 abort-chan 分离）。

(import (scheme base)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi code modes interactive state)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 验证获取非空通道，且与 agent 的 abort-chan 不是同一对象
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (ch (pi-app-bash-abort-chan app))
       (agent-ch (pi-app-abort-chan app))
      ) ;
  (check-true (chan? ch))
  (check-true (not (eq? ch agent-ch)))
) ;let*

(check-report)
