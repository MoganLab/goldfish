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

(import (scheme base) (liii go) (liii time))

;; demo/go_bug.scm: 展示当前 (liii go) 实现的根本硬伤 —— 线程级阻塞引发的任务饿死（Starvation Deadlock）
;;
;; 【问题根源】
;; 当前 C++ worker_loop 执行任务是同步阻塞的 s7_eval。
;; 当一个任务调用 (chan-recv! ch) 等待数据时，任务卡在底层的条件变量等待，
;; 导致其所在的整条 OS worker 线程被完全挂死，无法去线程池取下一个任务。
;;
;; 【复现方式】
;; 1. 获取当前系统分配的 worker 线程数 W = (go-worker-count)；
;; 2. 派发 W 个任务，每个任务都试图从空的 blocker 通道接收数据（导致所有 W 个 worker 线程全部陷入挂死）；
;; 3. 随后派发第 W+1 个任务：该任务原本负责向 blocker 发送数据解救大家；
;; 4. 【硬伤表现】：由于所有 worker 线程全被前序阻塞任务占满，第 W+1 个解救任务永远在线程池队列中排队，
;;    永远得不到执行机会，最终导致死锁超时！
;;
;; 【Go 语言对应行为】
;; 在 Go 中，goroutine 是 M:N 调度的用户态协程。前 W 个 goroutine 阻塞在 channel 时出让线程，
;; 第 W+1 个 goroutine 立即得到执行并发送数据，所有协程全部顺利完成，绝对不会死锁。

;; 阻塞任务：试图从空的通道读取数据，从而在当前 worker 线程中进入同步阻塞

(define (block-worker blocker)
  (chan-recv! blocker)
) ;define

;; 解救任务：向 blocker 发送唤醒信号，随后向 result-ch 汇报成功

(define (rescue-worker blocker result-ch)
  (chan-send! blocker 'wake-up)
  (chan-send! result-ch 'rescued)
) ;define

(define (main)
  (define workers (go-worker-count))
  (display (string-append "当前线程池 worker 线程数: "
             (number->string workers)
             "\n"
           ) ;string-append
  ) ;display

  (define blocker (make-chan))
  (define result-ch (make-chan 1))

  ;; 1. 派发 workers 个阻塞任务，把线程池中的所有 worker 线程全部占满
  (display (string-append "正在派发 "
             (number->string workers)
             " 个阻塞等待任务...\n"
           ) ;string-append
  ) ;display
  (let loop
    ((i 0))
    (when (< i workers)
      (go (block-worker blocker))
      (loop (+ i 1))
    ) ;when
  ) ;let

  ;; 稍作等待确保所有 worker 线程均已取出任务并进入阻塞状态
  (sleep 0.1)

  ;; 2. 派发第 workers + 1 个任务（解救任务）：向 blocker 发送数据唤醒大家
  (display "正在派发解救任务（负责向 blocker 发送数据唤醒大家）...\n"
  ) ;display
  (go (rescue-worker blocker result-ch))

  ;; 3. 主线程等待解救任务完成（设置 2000 毫秒超时）
  (display "主线程等待解救任务完成 (设置 2 秒超时检测)...\n")
  (let ((res (chan-recv! result-ch 2000 'timed-out)))
    (if (eq? res 'timed-out)
      (begin
        (display "====================================================================\n"
        ) ;display
        (display "【硬伤复现成功】解救任务被彻底饿死（无法被分配给任何 worker 线程执行）！\n"
        ) ;display
        (display "原因：所有 OS worker 线程均被挂死在 (chan-recv! blocker) 的 C++ 阻塞上，\n"
        ) ;display
        (display "      导致线程池完全瘫痪，无法执行队列中的后续任务。\n"
        ) ;display
        (display "====================================================================\n"
        ) ;display
      ) ;begin
      (display (string-append "解救成功: " (symbol->string res) "\n"))
    ) ;if
  ) ;let
) ;define

(main)
