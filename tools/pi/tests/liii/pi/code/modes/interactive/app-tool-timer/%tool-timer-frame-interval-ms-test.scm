;; %tool-timer-frame-interval-ms
;; 运行中 bash 工具 Elapsed 计时器刷新间隔（毫秒）常量。
;;
;; 语法
;; ----
;; %tool-timer-frame-interval-ms
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; integer?
;; 毫秒整数（默认 1000）。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive app-tool-timer)
) ;import

(check-set-mode! 'report-failed)

(check %tool-timer-frame-interval-ms => 1000)

(check-report)
