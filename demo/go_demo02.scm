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

;; 与 go_demo02.go 严格等价的 Goldfish Scheme (liii go) 版本：
;; 展示经典的并发流水线模式（Pipeline Pattern）：
;; 阶段 1 (stage1) 生成数据 -> 阶段 2 (stage2) 变换数据 -> 阶段 3 (main) 最终消费。

;; stage1 流水线阶段 1：数据生成者
;;
;; 对应 Go 版本的 func stage1(out chan<- int)
;; 使用 (numeric-range 1 6) 生成 1 到 5 的整数序列，依次写入通道 out-ch。
;; 发送完毕后通过 (chan-close! out-ch) 关闭通道，向下游发送结束信号。

(define (stage1 out-ch)
  (range-for-each (lambda (i) (chan-send! out-ch i)) (numeric-range 1 6))
  (chan-close! out-ch)
) ;define

;; stage2 流水线阶段 2：数据变换处理者（平方计算）
;;
;; 对应 Go 版本的 func stage2(in <-chan int, out chan<- int)
;; 使用 generator-for-each 从上游输入通道 in-ch 持续消费数据（当 in-ch 关闭时自动退出循环），
;; 将每个数据平方后写入下游输出通道 out-ch。
;; 上游排空后通过 (chan-close! out-ch) 关闭下游通道，继续传递结束信号。

(define (stage2 in-ch out-ch)
  (generator-for-each (lambda (v) (chan-send! out-ch (* v v)))
    (lambda () (chan-recv! in-ch))
  ) ;generator-for-each
  (chan-close! out-ch)
) ;define

;; consume 消费处理过程：处理单个接收到的平方数据

(define (consume v)
  (display "square: ")
  (display v)
  (newline)
) ;define

;; consumer 流水线阶段 3：最终通道消费者
;;
;; 对应 Go 版本的 for v := range squares { ... }
;; 使用 generator-for-each 结合 consume 消费最终通道 in-ch 中的数据。

(define (consumer in-ch)
  (generator-for-each consume (lambda () (chan-recv! in-ch)))
) ;define

;; main 主函数：组装并启动并发流水线
;;
;; 对应 Go 版本的 func main()

(define (main)
  ;; 1. 创建用于连接各个流水线阶段的通道 num-ch 和 square-ch
  (define num-ch (make-chan))
  (define square-ch (make-chan))

  ;; 2. 分别调度到后台 worker 线程并发执行各个流水线阶段：
  ;;    - (go (stage1 num-ch)) 在后台生成数据并写入 num-ch；
  ;;    - (go (stage2 num-ch square-ch)) 在后台从 num-ch 读取数据，计算平方后写入 square-ch。
  (go (stage1 num-ch))
  (go (stage2 num-ch square-ch))

  ;; 3. 阶段 3：在主线程作为最终消费者，消费并打印最终产物。
  (consumer square-ch)
) ;define

(main)
