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

(import (scheme base) (scheme time) (liii go))

(display "========================================================\n")
(display "  Goldfish (liii go) 2分钟多核挑战：能算到 fib 几？\n")
(display "  算法：基于 Go Channel 的树形分治并行 Fibonacci\n")
(display "========================================================\n\n")

(define workers (go-worker-count))
(display (string-append "检测到可用 CPU 工作核心数: "
           (number->string workers)
           "\n"
         ) ;string-append
) ;display

;; 压测持续时间：120 秒 (2 分钟)

(define target-duration-sec 120)

(display (string-append "挑战目标时长: "
           (number->string target-duration-sec)
           " 秒 (2 分钟)\n"
         ) ;string-append
) ;display
(display "并发机制：每阶 Fibonacci 树形展开为 32 个 Channel 并发叶子任务，20 个核心全负荷分治攻坚！\n"
) ;display
(display "请在另一个终端执行 htop 查看 CPU 占用（整整 2 分钟全部打满 2000%）！\n\n"
) ;display

;; 将一棵 Fibonacci 树展开指定深度，生成并发叶子任务列表

(define (expand-tree n depth)
  (if (or (<= depth 0) (<= n 1))
    (list n)
    (append (expand-tree (- n 1) (- depth 1)) (expand-tree (- n 2) (- depth 1)))
  ) ;if
) ;define

;; 适配 Go Channel 的树形分治并行算法

(define (parallel-fib n depth)
  (let* ((leaves (expand-tree n depth))
         (task-count (length leaves))
         (ch (make-chan task-count))
        ) ;
    ;; 派发所有叶子节点到 Go 线程池
    (for-each
      (lambda (sub-n)
        (go (ch sub-n)
          (letrec ((fib (lambda (x) (if (<= x 1) x (+ (fib (- x 1)) (fib (- x 2)))))))
            (chan-send! ch (fib sub-n))
          ) ;letrec
        ) ;go
      ) ;lambda
      leaves
    ) ;for-each
    ;; 收集并累加所有叶子结果
    (let collect
      ((i 0) (sum 0))
      (if (< i task-count) (collect (+ i 1) (+ sum (chan-recv! ch))) sum)
    ) ;let
  ) ;let*
) ;define

(define start-jiffy (current-jiffy))

(define target-jiffies (* target-duration-sec (jiffies-per-second)))

;; 从 35 阶开始冲刺（展开 5 层 = 32 个并发子任务）

(define current-n 35)

(define depth 5)

(define max-completed-n 0)

(define max-completed-val 0)

(display "多核分治引擎已全线开火，开始冲击更高阶 Fibonacci...\n\n"
) ;display

(let loop
  ()
  (let* ((now (current-jiffy)) (elapsed-jiffies (- now start-jiffy)))

    (if (< elapsed-jiffies target-jiffies)
      (let* ((t0 (current-jiffy))
             (ans (parallel-fib current-n depth))
             (t1 (current-jiffy))
             (cost (exact->inexact (/ (- t1 t0) (jiffies-per-second))))
             (total-elapsed (exact->inexact (/ (- t1 start-jiffy) (jiffies-per-second))))
             (rem-sec (truncate (max 0.0 (- target-duration-sec total-elapsed))))
            ) ;

        (set! max-completed-n current-n)
        (set! max-completed-val ans)

        (display (string-append "  [剩余 "
                   (number->string rem-sec)
                   "s | 耗时 "
                   (number->string cost)
                   "s] 攻克 fib("
                   (number->string current-n)
                   ") = "
                   (number->string ans)
                   "\n"
                 ) ;string-append
        ) ;display

        ;; 递增阶数继续冲刺
        (set! current-n (+ current-n 1))
        (loop)
      ) ;let*

      (display "\n2 分钟挑战时间到！正在生成统计战报...\n")
    ) ;if
  ) ;let*
) ;let

(define end-jiffy (current-jiffy))

(define total-sec
  (exact->inexact (/ (- end-jiffy start-jiffy) (jiffies-per-second)))
) ;define

(display "\n========================================================\n")
(display "  2 分钟多核极限冲刺完成！\n")
(display (string-append "实际总运行时间: " (number->string total-sec) " 秒\n")
) ;display
(display (string-append "利用 "
           (number->string workers)
           " 个核心，2 分钟内成功攻克的最大阶数是:\n\n"
         ) ;string-append
) ;display
(display (string-append "    >>>  fib("
           (number->string max-completed-n)
           ") = "
           (number->string max-completed-val)
           "  <<<\n\n"
         ) ;string-append
) ;display
(display "========================================================\n")
