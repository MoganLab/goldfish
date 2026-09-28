(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; chan-send!
;; 向通道发送一个值。
;;
;; 语法
;; ----
;; (chan-send! ch val)
;; (chan-send! ch val timeout-ms)
;;
;; 参数
;; ----
;; ch : channel
;; 目标通道。
;;
;; val : any
;; 要发送的值。支持数字、字符串、符号、布尔、字符、列表、vector、
;; bytevector、let 以及 channel 本身（深拷贝序列化传输，支持环与共享结构）。
;; 不支持过程/闭包。
;;
;; timeout-ms : integer?
;; 可选。超时毫秒数。缺省时无限等待（Go 语义）。
;;
;; 返回值
;; ----
;; boolean
;; 发送成功返回 #t；超时返回 #f。
;;
;; 错误处理
;; ----
;; ch 不是通道时抛出 type-error；向已关闭的通道发送抛出 value-error；
;; val 不可序列化时抛出 type-error。

(define ch (make-chan 2))
(check (chan-send! ch 42) => #t)
(check (chan-send! ch "hello") => #t)
(check (chan-recv! ch) => 42)
(check (chan-recv! ch) => "hello")

;; 缓冲满时带超时发送，超时返回 #f

(define ch-full (make-chan 1))
(chan-send! ch-full 'a)
(check (chan-send! ch-full 'b 50) => #f)
(check (chan-recv! ch-full) => 'a)
(check (chan-send! ch-full 'b 50) => #t)

;; 向已关闭通道发送抛出 value-error

(define ch-closed (make-chan 1))
(chan-close! ch-closed)
(check-catch 'value-error (chan-send! ch-closed 1))

;; 参数类型错误
(check-catch 'type-error (chan-send! "not-a-chan" 1))
;; 不可序列化的值（过程）
(check-catch 'type-error (chan-send! ch (lambda (x) x)))

(check-report)
