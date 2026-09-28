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

(import (scheme base) (scheme write) (liii go))

;; 与 09_random_select.go 严格等价的 Goldfish Scheme (liii go) 版本：
;; 展示 select 宏底层基于 Fisher-Yates 洗牌算法实现的随机公平性（Random Fairness）：
;; 当多个 case 同时处于就绪状态时，select 会以伪随机均匀概率选择其中一个执行，
;; 避免按声明顺序扫描导致的后序分支饥饿（Starvation）现象。

(define (main)
  ;; ch-a 与 ch-b 均为容量为 1 的缓冲通道，且初始都塞满数据（均处于可读就绪状态）
  (define ch-a (make-chan 1))
  (define ch-b (make-chan 1))
  (chan-send! ch-a "Channel-A")
  (chan-send! ch-b "Channel-B")

  (define count-a 0)
  (define count-b 0)
  (define rounds 20)

  (display "=== 两个分支同时常态就绪时的 20 次 select 效果 (Goldfish Scheme) ===\n"
  ) ;display
  (let loop
    ((i 1))
    (when (<= i rounds)
      (select
       ((chan-recv! ch-a v)
        (set! count-a (+ count-a 1))
        (display "Round ")
        (if (< i 10) (display " "))
        (display i)
        (display ": 选中了 ")
        (display v)
        (newline)
        (chan-send! ch-a "Channel-A")
       ) ;
       ((chan-recv! ch-b v)
        (set! count-b (+ count-b 1))
        (display "Round ")
        (if (< i 10) (display " "))
        (display i)
        (display ": 选中了 ")
        (display v)
        (newline)
        (chan-send! ch-b "Channel-B")
       ) ;
      ) ;select
      (loop (+ i 1))
    ) ;when
  ) ;let

  (display "\n统计结果: Channel-A 选中 ")
  (display count-a)
  (display " 次, Channel-B 选中 ")
  (display count-b)
  (display " 次\n")
) ;define

(main)
