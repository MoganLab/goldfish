;; %working-frame-interval-ms
;; 工作指示器动画帧间隔时间（毫秒）常量。
;;
;; 语法
;; ----
;; %working-frame-interval-ms
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; integer?
;; 毫秒整数（默认 80）。

(import (scheme base) (liii check) (liii pi code modes interactive working))

(check-set-mode! 'report-failed)

(check %working-frame-interval-ms => 80)

(check-report)
