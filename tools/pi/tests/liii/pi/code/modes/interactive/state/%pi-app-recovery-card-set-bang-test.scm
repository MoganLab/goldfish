;; %pi-app-recovery-card-set!
;; 设置当前 all-in-one 全链等待状态卡片（#f 表示未在等待）。
;;
;; 语法
;; ----
;; (%pi-app-recovery-card-set! app val)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; val : (or component #f)
;; 等待状态卡片组件；#f 清除。
;;
;; 返回值
;; ----
;; unspecified
;; 仅更新状态字段，不触发渲染。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
  (liii pi code modes interactive status-card)
) ;import

(check-set-mode! 'report-failed)

;; 1. 写入后可读回
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (card (make-status-card "Retrying in 60s"))
      ) ;
  (%pi-app-recovery-card-set! app card)
  (check (pi-app-recovery-card app) => card)
) ;let*

;; 2. 连续覆盖只保留最后一张卡片
(let* ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)))
       (a (make-status-card "a"))
       (b (make-status-card "b"))
      ) ;
  (%pi-app-recovery-card-set! app a)
  (%pi-app-recovery-card-set! app b)
  (check (pi-app-recovery-card app) => b)
) ;let*

(check-report)
