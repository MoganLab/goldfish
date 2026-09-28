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

;; 与 05_context_cancel.go 严格等价的 Goldfish Scheme (liii go) 版本：
;; 展示在任务通道故意未关闭的情况下，如何依靠 context 上下文机制优雅取消并退出后台 worker。

;; worker 后台任务协程：
;; 从 jobs 通道读取数据并翻倍处理写入 out 通道；
;; 同时监听 (context-channel ctx) 取消信号，一旦收到信号立即退出并关闭 out 通道通知下游。

(define (worker ctx jobs out)
  (let loop
    ()
    (select ((chan-recv! (context-channel ctx))
             =>
             (lambda (_) (display "worker exit by ctx\n") (chan-close! out))
            ) ;
     ((chan-recv! jobs) => (lambda (j) (chan-send! out (* j 2)) (loop)))
    ) ;select
  ) ;let
) ;define

(define (main)
  ;; jobs: 缓冲容量为 3，发送 3 个任务后故意不关闭，演示在任务流未显式结束时靠 ctx 取消优雅退出
  (define jobs (make-chan 3))
  ;; out: 缓冲容量为 3，避免 worker 发送结果时阻塞
  (define out (make-chan 3))

  ;; 创建可取消的上下文 context
  (define ctx (make-context))
  (go (worker ctx jobs out))

  ;; 向 jobs 发送 3 个任务
  (chan-send! jobs 1)
  (chan-send! jobs 2)
  (chan-send! jobs 3)

  ;; 等待 50 毫秒让 worker 处理完任务，随后主动取消上下文
  (sleep 0.05)
  (context-cancel! ctx)

  ;; 主协程通过 generator 消费等待 out 通道关闭（由 worker 退出时负责 close）
  (generator-for-each (lambda (v) (display "result: ") (display v) (newline))
    (lambda () (chan-recv! out))
  ) ;generator-for-each
) ;define

(main)
