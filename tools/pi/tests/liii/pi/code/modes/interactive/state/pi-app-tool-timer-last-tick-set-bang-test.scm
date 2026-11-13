;; pi-app-tool-timer-last-tick-set!
;; 设置应用实例记录的运行中 bash 工具 Elapsed 计时器上次重绘时间戳。
;;
;; 语法
;; ----
;; (pi-app-tool-timer-last-tick-set! app tick)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; tick : number? 或 #f
;; 新的时间戳数值或 #f。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (pi-app-tool-timer-last-tick-set! app 12345678)
  (check (pi-app-tool-timer-last-tick app) => 12345678)
  (pi-app-tool-timer-last-tick-set! app #f)
  (check (pi-app-tool-timer-last-tick app) => #f)
) ;let

(check-report)
