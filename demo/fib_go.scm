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

(import (scheme base)
        (scheme time)
        (liii go))

(display "========================================================\n")
(display "  Goldfish CSP (liii go) 多核并发 Fibonacci 压力测试\n")
(display "========================================================\n\n")

(define workers (go-worker-count))
(display (string-append "检测到可用 CPU 工作核心数: " (number->string workers) "\n"))

;; 任务数量设为工作核心数的两倍，确保每个核心持续打满
(define task-count (* workers 2))
(define fib-target 34)

(display (string-append "准备启动 " (number->string task-count)
                        " 个并发任务，每个任务独立递归计算 fib("
                        (number->string fib-target) ")...\n"))
(display "此时所有 CPU 核心将瞬间跑满 100%！\n\n")

(define results (make-chan task-count))
(define start-time (current-jiffy))

;; 启动所有并发 Worker
(let loop ((i 1))
  (if (<= i task-count)
      (begin
        (go (results i fib-target)
          ;; 在独立解释器环境中纯 CPU 密集型递归计算
          (letrec ((fib (lambda (n)
                          (if (<= n 1)
                              n
                              (+ (fib (- n 1)) (fib (- n 2)))))))
            (let ((ans (fib fib-target)))
              (chan-send! results (list i ans)))))
        (loop (+ i 1)))))

(display "所有并发任务已分发到线程池，正在全力并行计算中...\n")

;; 收集所有任务结果
(let collect ((i 1))
  (if (<= i task-count)
      (let ((res (chan-recv! results)))
        (display (string-append "  [完成] 任务 #" (number->string (car res))
                                " -> fib(" (number->string fib-target) ") = "
                                (number->string (cadr res)) "\n"))
        (collect (+ i 1)))))

(define end-time (current-jiffy))
(define elapsed-sec (exact->inexact (/ (- end-time start-time) (jiffies-per-second))))

(display "\n--------------------------------------------------------\n")
(display (string-append "全部 " (number->string task-count)
                        " 个高密集计算任务完成！总耗时: "
                        (number->string elapsed-sec) " 秒\n"))
(display (string-append "总吞吐率: "
                        (number->string (/ task-count elapsed-sec)) " 任务/秒\n"))
(display "========================================================\n")
