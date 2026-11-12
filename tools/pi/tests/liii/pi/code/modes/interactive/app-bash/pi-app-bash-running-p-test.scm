;; pi-app-bash-running?
;; 判断交互式应用是否有 ! 命令在运行（含已派发尚未开始执行的排队命令）。
;;
;; 语法
;; ----
;; (pi-app-bash-running? app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; boolean?
;; 任一 bash 卡片处于 running 状态时 #t；ESC 中止 ! 命令与退出中止的门控
;; （对齐上游 isBashRunning → abortBash）。

(import (scheme base)
  (liii check)
  (liii go)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. 无命令时为 #f
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (check-true (not (pi-app-bash-running? app)))
) ;let*

;; 2. 注入同步 bash-handler：pi-app-run-bash! 内同步结算，卡片不留在 running
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm
              :bash-handler (lambda (cmd) (values "output" 0))
            ) ;make-pi-app
      ) ;app 绑定
      ) ;绑定列表
  (check-true (pi-app-run-bash! app "echo hi"))
  (check-true (not (pi-app-bash-running? app)))
) ;let*

;; 3. 生产路径派发后为 #t：sleep 1s 的命令在检查时刻必然仍在运行
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (check-true (pi-app-run-bash! app "sleep 1"))
  (check-true (pi-app-bash-running? app))
) ;let*

(check-report)
