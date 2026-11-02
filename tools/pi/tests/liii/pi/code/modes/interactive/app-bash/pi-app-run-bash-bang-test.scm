;; pi-app-run-bash!
;; 交互式应用中调度执行单条 Bash 命令。
;;
;; 语法
;; ----
;; (pi-app-run-bash! app command . opts)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; command : string
;; 待执行的 shell 命令字符串。
;; opts : any
;; 可选关键字参数（如 :excluded?）。
;;
;; 返回值
;; ------
;; any
;; 执行命令分发并返回发送结果。

(import (scheme base)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 执行 bash 命令分发，聊天列表新增卡片
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
       (res (pi-app-run-bash! app "echo hello"))
      ) ;
  (check-true res)
  (check-true (pair? (pi-app-chat app)))
) ;let*

(check-report)
