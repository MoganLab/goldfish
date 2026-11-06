;; chan-send!
;; 通道传输的集成测试：可序列化类型的完整读写矩阵，以及 channel 作为
;; 一等公民在通道之间传递的往返验证。各函数的独立文档与基础用例见
;; 同目录 <函数名>-test.scm。
;;
;; 语法
;; ----
;; (chan-send! ch val)
;; (chan-recv! ch)
;;
;; 参数
;; ----
;; ch : channel
;; 收发送消息的通道。
;; val : any
;; 待传输的消息，必须为可序列化的数据类型。
;;
;; 返回值
;; ----
;; any
;; chan-recv! 返回通道中的下一个值。

(import (liii check) (liii go))

(check-set-mode! 'report-failed)

;; 1. 全类型读写矩阵（深拷贝序列化往返）

(define ch2 (make-chan 20))
(chan-send! ch2 42)
(chan-send! ch2 3.14)
(chan-send! ch2 #t)
(chan-send! ch2 #f)
(chan-send! ch2 #\a)
(chan-send! ch2 "hello world")
(chan-send! ch2 'my-symbol)
(chan-send! ch2 '())
(chan-send! ch2 '(1 2 3 (4 5)))
(chan-send! ch2 #(10 20 30))
(chan-send! ch2 #u8(1 2 3 255))

(check (chan-recv! ch2) => 42)
(check (chan-recv! ch2) => 3.14)
(check (chan-recv! ch2) => #t)
(check (chan-recv! ch2) => #f)
(check (chan-recv! ch2) => #\a)
(check (chan-recv! ch2) => "hello world")
(check (chan-recv! ch2) => 'my-symbol)
(check (chan-recv! ch2) => '())
(check (chan-recv! ch2) => '(1 2 3 (4 5)))
(check (chan-recv! ch2) => #(10 20 30))
(check (chan-recv! ch2) => #u8(1 2 3 255))

;; 2. 通道本身作为消息在另一个通道中传递（first-class channel）

(define meta-ch (make-chan 2))

(define sub-ch (make-chan 2))
(chan-send! sub-ch "secret")
(chan-send! meta-ch sub-ch)

(define received-sub-ch (chan-recv! meta-ch))
(check (chan? received-sub-ch) => #t)
(check (chan-recv! received-sub-ch) => "secret")

(check-report)
