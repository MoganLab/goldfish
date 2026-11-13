;; pi-app-has-pending-bash-tool?
;; 判定当前应用中是否存在正在运行（pending 状态）的 bash 工具调用。
;;
;; 语法
;; ----
;; (pi-app-has-pending-bash-tool? app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; boolean?
;; 若存在运行中的 bash 工具返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive views)
  (liii pi code modes interactive app-tool-timer)
  (liii pi code modes interactive worker-emit)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始状态：无任何工具调用，返回 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check-false (pi-app-has-pending-bash-tool? app))
) ;let

;; 2. 收到 bash 工具开始事件：返回 #t
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (emit (make-tui-emit (pi-app-event-chan app)))
      ) ;
  (emit 'tool_execution_start "t-1" "bash" '(("command" . "sleep 5")))
  (pi-app-poll-events! app)
  (check-true (pi-app-has-pending-bash-tool? app))
  ;; 工具执行结束：返回 #f
  (emit 'tool_execution_end "t-1" "bash" "done" #f 5000)
  (pi-app-poll-events! app)
  (check-false (pi-app-has-pending-bash-tool? app))
) ;let*

;; 3. 非 bash 工具（如 read/write）：即使 pending 也返回 #f
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (emit (make-tui-emit (pi-app-event-chan app)))
      ) ;
  (emit 'tool_execution_start "t-2" "read" '(("path" . "a.scm")))
  (pi-app-poll-events! app)
  (check-false (pi-app-has-pending-bash-tool? app))
) ;let*

;; 4. 直接加入 chat 容器的 pending bash 卡片也能正确识别
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (card (make-tool-card "bash" '((command . "ls")) 'pending))
      ) ;
  (container-add! (%pi-app-chat app) card)
  (check-true (pi-app-has-pending-bash-tool? app))
  (tool-card-set-complete! card "output" #f 100)
  (check-false (pi-app-has-pending-bash-tool? app))
) ;let*

(check-report)
