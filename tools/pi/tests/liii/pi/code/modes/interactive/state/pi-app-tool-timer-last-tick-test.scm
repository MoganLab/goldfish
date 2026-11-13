;; pi-app-tool-timer-last-tick
;; 获取应用实例记录的运行中 bash 工具 Elapsed 计时器上次重绘时间戳。
;;
;; 语法
;; ----
;; (pi-app-tool-timer-last-tick app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; number? 或 #f
;; 计时器上次节拍时间戳；未处于计时状态时为 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始状态：未处于计时状态，返回 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (pi-app-tool-timer-last-tick app) => #f)
) ;let

(check-report)
