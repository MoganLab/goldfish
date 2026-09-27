;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(import (scheme base) (liii go) (liii range) (liii generator))

;; 与 go_demo01.go 严格等价的 Goldfish Scheme (liii go) 版本：
;; 展示基于 CSP 并发模型的生产者-消费者模式。

;; consume 消费处理函数：处理单个接收到的数据

(define (consume v)
  (display "consume: ")
  (display v)
  (newline)
) ;define

;; consumer 消费者函数：从通道接收并打印数据
;;
;; 对应 Go 版本的 func consumer(ch <-chan int)
;;
;; 机制说明：
;; 在 Go 中使用 `for v := range ch` 持续迭代消费通道，直到通道被关闭且排空。
;; 在 Scheme (SRFI 158) 体系中，流式迭代的核心抽象是生成器（generator）：
;; 生成器是一个无参过程，每次调用返回下一个值，耗尽时返回 eof-object。
;; (chan-recv! ch) 在通道关闭且排空时刚好返回 eof-object，
;; 因此 `(lambda () (chan-recv! ch))` 就是一个天然的 Generator！
;; 将提取出的 consume 函数传入 generator-for-each，
;; 与 Go 的 `for-range` 通道遍历语义完全等价且极其优雅。

(define (consumer ch)
  (generator-for-each consume (lambda () (chan-recv! ch)))
) ;define

;; main 主入口函数
;;
;; 对应 Go 版本的 func main()

(define (main)
  ;; 1. 创建一个无缓冲通道（capacity = 0）
  (define ch (make-chan))

  ;; 2. 启动后台 worker 执行生产者
  ;;    对应 Go 语言中的 go producer(ch)。
  ;;
  ;;    producer 是在 go 协程里面执行的函数：
  ;;    Goldfish Scheme 的 worker 运行在相互隔离的独立解释器环境中，
  ;;    因此在 go 任务块内定义 producer 函数并在 worker 线程中调用执行。
  (go (ch)
    (define (producer ch)
      (import (liii range))
      ;; 依次发送数字 1 到 5：(numeric-range 1 6) 生成 [1, 6) 的整数序列
      (range-for-each (lambda (i)
                        ;; 由于 ch 是无缓冲通道（make-chan 默认容量 0），
                        ;; chan-send! 会阻塞直到有接收方调用 chan-recv!（同步交会 rendezvous）
                        (chan-send! ch i)
                      ) ;lambda
        (numeric-range 1 6)
      ) ;range-for-each
      ;; 发送完毕，由生产者负责关闭通道（对应 Go 中的 defer close(ch)）
      (chan-close! ch)
    ) ;define
    (producer ch)
  ) ;go

  ;; 3. 在当前主线程同步执行消费者，消费完毕后退出
  (consumer ch)
) ;define

(main)
