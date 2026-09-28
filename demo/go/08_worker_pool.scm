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

(import (scheme base) (liii go) (liii generator) (liii time))

;; 与 08_worker_pool.go 严格等价的 Goldfish Scheme (liii go) 版本：
;; 展示经典的工作池模式（Worker Pool）：固定数量的 worker 协程并发竞争消费任务通道。

;; worker 工作协程：
;; 并发从 jobs 通道中抢占任务，计算翻倍后将结果送入 results 通道。
;; 当 jobs 通道关闭且排空后，generator-for-each 自动退出。

(define (worker id jobs results)
  (generator-for-each
    (lambda (j)
      (display (string-append "[worker "
                 (number->string id)
                 "] 开始处理任务: "
                 (number->string j)
                 "\n"
               ) ;string-append
      ) ;display
      (sleep 0.02)
      (chan-send! results (* j 2))
    ) ;lambda
    (lambda () (chan-recv! jobs))
  ) ;generator-for-each
) ;define

(define (main)
  (define num-jobs 5)
  (define jobs (make-chan num-jobs))
  (define results (make-chan num-jobs))

  ;; 1. 启动 3 个 worker 协程组成工作池（并发限流为 3）
  (go (worker 1 jobs results))
  (go (worker 2 jobs results))
  (go (worker 3 jobs results))

  ;; 2. 发送 5 个任务，发送完毕后关闭 jobs 通道
  (let loop
    ((j 1))
    (when (<= j num-jobs)
      (chan-send! jobs j)
      (loop (+ j 1))
    ) ;when
  ) ;let
  (chan-close! jobs)

  ;; 3. 收集所有任务的处理结果
  (let loop
    ((a 1))
    (when (<= a num-jobs)
      (display (string-append "收到结果: " (number->string (chan-recv! results)) "\n")
      ) ;display
      (loop (+ a 1))
    ) ;when
  ) ;let
) ;define

(main)
