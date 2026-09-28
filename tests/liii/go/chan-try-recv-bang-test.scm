(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; chan-try-recv!
;; 非阻塞地从通道接收一个值。
;;
;; 语法
;; ----
;; (chan-try-recv! ch)
;; (chan-try-recv! ch default)
;;
;; 参数
;; ----
;; ch : channel
;; 源通道。
;;
;; default : any
;; 可选。通道为空时返回的默认值，缺省为 #f。
;;
;; 返回值
;; ----
;; any
;; 通道非空时返回下一个值；为空时立即返回 default；
;; 通道已关闭且读空时返回 eof-object。

(define ch (make-chan 2))
(check (chan-try-recv! ch) => #f)
(check (chan-try-recv! ch 'empty) => 'empty)

(chan-send! ch 'a)
(check (chan-try-recv! ch) => 'a)
(check (chan-try-recv! ch) => #f)

(chan-close! ch)
(check (eof-object? (chan-try-recv! ch)) => #t)

(check-report)
