(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; 1. make-chan 与 chan? 谓词测试
(define ch1 (make-chan))
(check (chan? ch1) => #t)
(check (chan? 123) => #f)
(check (chan? "channel") => #f)
(check (chan? '()) => #f)

;; 2. 有缓冲 channel 测试：基本类型读写
(define ch2 (make-chan 20))
(check (chan? ch2) => #t)

;; 写入各种数据类型
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

;; 依次读出并验证
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

;; 3. 超时机制测试（每次运行提供超时时间，杜绝死等）
(define ch-timeout (make-chan 1))
;; 接收超时
(check (chan-recv! ch-timeout 50 'my-timeout) => 'my-timeout)
;; 填满 channel
(check (chan-send! ch-timeout "only-one" 50) => #t)
;; 发送超时
(check (chan-send! ch-timeout "overflow" 50) => #f)
;; 取出后又可以发送
(check (chan-recv! ch-timeout 50) => "only-one")
(check (chan-send! ch-timeout "second" 50) => #t)
(check (chan-recv! ch-timeout 50) => "second")

;; 3. chan-try-recv! 非阻塞读取测试
(define ch3 (make-chan 2))
;; 此时为空
(check (chan-try-recv! ch3) => #f)
(check (chan-try-recv! ch3 'empty) => 'empty)

(chan-send! ch3 "first")
(check (chan-try-recv! ch3) => "first")
;; 再次变为空
(check (chan-try-recv! ch3) => #f)

;; 4. chan-close! 与 chan-closed? 测试
(define ch4 (make-chan 5))
(check (chan-closed? ch4) => #f)

(chan-send! ch4 100)
(chan-send! ch4 200)
(chan-close! ch4)

(check (chan-closed? ch4) => #t)

;; 关闭后已有的数据仍可正常读出
(check (chan-recv! ch4) => 100)
(check (chan-recv! ch4) => 200)

;; 读空后再次读，返回 eof-object
(define eof-res (chan-recv! ch4))
(check (eof-object? eof-res) => #t)

;; chan-try-recv! 在读空且关闭时也返回 eof-object
(check (eof-object? (chan-try-recv! ch4)) => #t)

;; 重复 close 是安全的
(chan-close! ch4)
(check (chan-closed? ch4) => #t)

;; 5. 向已关闭通道发送应报错
(check-catch 'value-error (chan-send! ch4 300))

;; 6. 通道本身作为消息在另一个通道中传递 (First-class Channel)
(define meta-ch (make-chan 2))
(define sub-ch (make-chan 2))
(chan-send! sub-ch "secret")
(chan-send! meta-ch sub-ch)

(define received-sub-ch (chan-recv! meta-ch))
(check (chan? received-sub-ch) => #t)
(check (chan-recv! received-sub-ch) => "secret")

;; 7. 参数类型错误检查
(check-catch 'type-error (chan-send! "not-a-chan" 1))
(check-catch 'type-error (chan-recv! "not-a-chan"))
(check-catch 'type-error (chan-try-recv! "not-a-chan"))
(check-catch 'type-error (chan-close! "not-a-chan"))
(check-catch 'type-error (chan-closed? "not-a-chan"))

(check-report)
