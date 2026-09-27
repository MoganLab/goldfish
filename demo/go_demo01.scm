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

(import (scheme base) (liii go) (liii range) (liii generator) (liii time))

;; 与 go_demo01.go 严格等价的 Goldfish Scheme (liii go) 版本：
;; 演示不同缓冲容量（0, 1, 3）的通道在生产与消费过程中的阻塞与交会行为。

;; producer 生产者：向通道发送数据
;;
;; 参数说明：
;; - name: 演示通道名称（用于日志前缀区分）
;; - ch: 目标通道
;; - count: 发送的数据总数（从 1 发送到 count）
;;
;; 通过在发送前后打印日志，可清晰观察何时发生阻塞：
;; - 若通道有空余缓冲空间（或有接收者在等待），发送立即完成；
;; - 若通道无缓冲且无接收者，或缓冲已满，则当前 worker 协程阻塞在 chan-send! 处。

(define (producer name ch count)
  (range-for-each
    (lambda (i)
      (display (string-append "[" name " producer] 准备发送: " (number->string i) "\n")
      ) ;display
      (chan-send! ch i)
      (display (string-append "[" name " producer] 发送成功: " (number->string i) "\n")
      ) ;display
    ) ;lambda
    (numeric-range 1 (+ count 1))
  ) ;range-for-each
  (chan-close! ch)
  (display (string-append "["
             name
             " producer] 全部数据发送完毕，通道已关闭\n"
           ) ;string-append
  ) ;display
) ;define

;; consume 消费处理过程：处理单个接收到的数据
;;
;; 模拟耗时处理（50 毫秒），使缓冲被填满的情景更容易观察

(define (consume name v)
  (sleep 0.05)
  (display (string-append "[" name " consumer] 成功接收: " (number->string v) "\n")
  ) ;display
) ;define

;; consumer 消费者：从通道接收并处理数据（整体消费阶段）
;;
;; (lambda () (chan-recv! ch)) 是天然的 Generator，配合 generator-for-each 调用 consume 优雅消费。

(define (consumer name ch)
  (generator-for-each (lambda (v) (consume name v)) (lambda () (chan-recv! ch)))
  (display (string-append "[" name " consumer] 检测到通道关闭，消费结束\n")
  ) ;display
) ;define

;; run-demo 演示指定通道的生产与消费行为

(define (run-demo name ch capacity)
  (display (string-append "=== 演示 "
             name
             " (缓冲容量: "
             (number->string capacity)
             ") ===\n"
           ) ;string-append
  ) ;display
  ;; 启动后台生产者 worker 协程
  (go (producer name ch 5))
  ;; 主线程作为消费者同步消费
  (consumer name ch)
  (newline)
) ;define

;; main 主入口函数
;;
;; 对应 Go 版本的 func main()

(define (main)
  ;; ch0: 无缓冲通道（容量为 0）
  ;; 发送与接收必须同步交会（rendezvous）：发送者会阻塞直到消费者接收
  (define ch0 (make-chan))
  (run-demo "ch0" ch0 0)

  ;; ch1: 1 个缓冲的通道
  ;; 缓冲区可容纳 1 个元素：第 1 个元素发送不会阻塞，发第 2 个元素时若未被取走则阻塞
  (define ch1 (make-chan 1))
  (run-demo "ch1" ch1 1)

  ;; ch3: 3 个缓冲的通道
  ;; 缓冲区可容纳 3 个元素：前 3 个元素连续发送不阻塞，发第 4 个元素时缓冲已满而阻塞
  (define ch3 (make-chan 3))
  (run-demo "ch3" ch3 3)
) ;define

(main)
