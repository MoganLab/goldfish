(import (liii check) (liii time))


(check-set-mode! 'report-failed)


;; time-resolution
;; 获取时钟分辨率。
;;
;; 语法
;; ----
;; (time-resolution [clock-type])
;;
;; 参数
;; ----
;; clock-type : symbol? (可选) 时钟类型，默认为 TIME-UTC。
;;
;; 返回值
;; ----
;; integer? 一个纳秒整数，表示指定时钟的分辨率（精度）。
;;
;; 错误处理
;; ----
;; type-error 当clock-type不是有效的时间类型常量时抛出错误。


;; Test time-resolution
(check-true (integer? (time-resolution)))
(check-true (integer? (time-resolution TIME-UTC)))
(check-true (integer? (time-resolution TIME-MONOTONIC)))
(check-true (integer? (time-resolution TIME-TAI)))
(check-catch 'type-error (time-resolution TIME-THREAD))
(check-catch 'type-error (time-resolution TIME-PROCESS))
(check-catch 'type-error (time-resolution TIME-DURATION))


;; Test error conditions
(check-catch 'type-error (time-resolution 'invalid-type))


(check-report)
