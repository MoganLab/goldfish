(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; chan-closed?
;; 判断通道是否已关闭。
;;
;; 语法
;; ----
;; (chan-closed? ch)
;;
;; 参数
;; ----
;; ch : channel
;; 目标通道。
;;
;; 返回值
;; ----
;; boolean
;; 通道已关闭返回 #t，否则返回 #f。
;;
;; 错误处理
;; ----
;; ch 不是通道时抛出 type-error。

(define ch (make-chan 1))
(check (chan-closed? ch) => #f)
(chan-close! ch)
(check (chan-closed? ch) => #t)

(check-catch 'type-error (chan-closed? "not-a-chan"))

(check-report)
