;; pi-tui-agent-worker
;; 启动交互模式后台 Agent Worker 线程循环处理 jobs-chan 任务。
;;
;; 语法
;; ----
;; (pi-tui-agent-worker jobs-chan event-chan abort-chan parsed cfg)
;;
;; 参数
;; ----
;; jobs-chan : chan?
;; 待执行任务接收通道。
;; event-chan : chan?
;; 事件投递通道。
;; abort-chan : chan?
;; 中断控制通道。
;; parsed : list?
;; 命令行参数。
;; cfg : list?
;; 配置列表。
;;
;; 返回值
;; ----
;; any
;; worker 过程或退出状态。

(import (scheme base)
  (liii check)
  (liii go)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 1. jobs-chan 关闭时 Worker 正常结束循环退出
(let* ((jobs-ch (make-chan 4)) (ev-ch (make-chan 4)) (abort-ch (make-chan 4)))
  (chan-close! jobs-ch)
  (pi-tui-agent-worker jobs-ch ev-ch abort-ch)
  (check-true #t)
) ;let*

(check-report)
