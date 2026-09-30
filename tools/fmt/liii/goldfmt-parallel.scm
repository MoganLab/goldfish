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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; gf fmt 的共享并发设施：(liii goldfmt-parallel)。
;; 基于 (liii go)（CSP：每 worker 线程独占独立 s7 解释器会话，channel 传深拷贝
;; 消息）的极简 Worker Pool（同 gf test -j 的双通道模式）：
;;   1. pool-ch / result-ch 容量均为文件数（发送永不阻塞）；
;;   2. 主线程串行投递任务（消息体就是文件路径字符串），投递完毕 chan-close!
;;      广播 EOF 哨兵；
;;   3. 启动 n = min(jobs, 文件数) 个 worker 循环消费；
;;   4. worker 把 (list 路径 状态 失败信息) 回发 result-ch，主线程按到达顺序
;;      即时回收并回调——不保证完成顺序，无需保序缓冲。
;; 语言之间保持串行（由主入口的逐语言循环保证），本模块只做语言内并行。
;; 并发度参数（-j/--jobs 的解析结果）也寄居于此：所有消费方（主入口与三个
;; 语言的批量层）都已 import 本模块。

(define-library (liii goldfmt-parallel)
  (import (liii base) (liii go) (liii list))
  (export pool-for-each serial-for-each run-worker-loop offenders-from
    count-status set-fmt-jobs! fmt-jobs
  ) ;export
  (begin

    ;; ---- 并发度参数 -----------------------------------------------------
    ;; gf fmt 语言内文件级并发的 worker 数：主入口解析 -j/--jobs 后设置一次，
    ;; 各语言的批量格式化层读取（worker 侧不需要读取）。1 表示串行；
    ;; 以库形式直接使用时默认 1（保持串行，行为向后兼容）。
    (define %fmt-jobs 1)

    ;; 设置并发度：仅接受不小于 1 的数，其余回退 1。
    (define (set-fmt-jobs! n)
      (set! %fmt-jobs (if (and (number? n) (>= n 1)) n 1))
    ) ;define

    ;; 读取并发度。
    (define (fmt-jobs)
      %fmt-jobs
    ) ;define

    ;; ---- worker 骨架 ----------------------------------------------------
    ;; 在 worker 会话内循环取任务、调单位函数、回发结果，读到 EOF 后退出。
    ;; 任务消息体就是文件路径字符串；结果回发 (list 路径 状态 失败信息)。
    ;; unit-fn: (lambda (path) ...) 返回 (status msg) 二元表；
    ;; err-fn: (lambda (tag info) ...) 在单位函数抛异常时构造 (status msg)，
    ;; 必须兜住一切异常并返回结果（否则主线程会因收不齐结果而永久等待）。
    ;; 供各语言的 worker 包装调用：worker 函数经 (go ...) ship 的只有自身
    ;; 源码，其引用的本骨架与单位函数等符号必须是导出符号（或以内联
    ;; lambda 的形式写在其函数体内）。
    (define (run-worker-loop task-ch result-ch unit-fn err-fn)
      (let loop
        ()
        (let ((task (chan-recv! task-ch)))
          (unless (eof-object? task)
            (let ((r (catch #t (lambda () (unit-fn task)) (lambda (tag info) (err-fn tag info))))
                 ) ;
              (chan-send! result-ch (list task (car r) (cadr r)))
              (loop)
            ) ;let
          ) ;unless
        ) ;let
      ) ;let
    ) ;define

    ;; ---- 串行孪生 -------------------------------------------------------
    ;; 与 Worker Pool 同形的串行实现：同一单位函数、同一回调、同一返回值
    ;; 形状（结果同样为 (list 路径 状态 失败信息)，串行天然按文件序到达），
    ;; 供 -j 1 / 工作量太小时复用批量层其余部分。
    (define (serial-for-each unit-fn files on-result)
      (let loop
        ((fs files) (acc '()))
        (if (null? fs)
          (reverse acc)
          (let* ((r (unit-fn (car fs))) (msg (list (car fs) (car r) (cadr r))))
            (on-result (car fs) msg)
            (loop (cdr fs) (cons msg acc))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; ---- Worker Pool ----------------------------------------------------
    ;; worker-fn: 各语言导出的 (lambda (worker-args ... pool-ch result-ch))，
    ;;   经 (liii go) 派发到独立 worker 会话（内部用 %go-call 按可选前置参数
    ;;   动态构造调用——go 宏要求直接调用语法，无法在驱动内按语言拼装）。
    ;; worker-args: 该语言 worker 的前置初始化实参（如 cpp 的 clang-format
    ;;   二进制路径字符串），spawn 时一次性序列化传入。
    ;; on-result: (lambda (file result))，result 为 (list 路径 状态 失败信息)，
    ;;   按到达顺序即时回调（流式输出，不保证完成顺序）。
    ;; 返回按到达顺序的结果列表（(list 路径 状态 失败信息) ...），统计与
    ;;   offenders 提取均与其顺序无关。
    (define (pool-for-each worker-fn files on-result . worker-args)
      (let* ((total (length files))
             (pool-ch (make-chan total))
             (result-ch (make-chan total))
             (jobs (min (fmt-jobs) total))
            ) ;
        ;; 1. 串行投递全部任务（文件路径），然后关闭任务通道。
        (let enq
          ((fs files))
          (unless (null? fs)
            (chan-send! pool-ch (car fs))
            (enq (cdr fs))
          ) ;unless
        ) ;let
        (chan-close! pool-ch)
        ;; 2. 启动 jobs 个 worker（实际并发度 = min(jobs, 文件数)）。
        (let spawn
          ((i 0))
          (when (< i jobs)
            (apply %go-call worker-fn (append worker-args (list pool-ch result-ch)))
            (spawn (+ i 1))
          ) ;when
        ) ;let
        ;; 3. 按到达顺序收满 total 个结果，即时回调。
        (let recv
          ((got 0) (acc '()))
          (if (= got total)
            (reverse acc)
            (let ((r (chan-recv! result-ch)))
              (on-result (car r) r)
              (recv (+ got 1) (cons r acc))
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    ;; ---- 结果归约辅助 ---------------------------------------------------
    ;; 统计结果列表中某状态的数量（结果为 (list 路径 状态 失败信息)，
    ;; 顺序无关）。
    (define (count-status sym results)
      (count (lambda (r) (eq? (cadr r) sym)) results)
    ) ;define

    ;; 由 check 的结果列表提取未格式化文件（顺序无关）：
    ;; 每个结果为 (路径 ok 失败信息)，ok 为 #f 的路径进入 offenders。
    (define (offenders-from results)
      (let loop
        ((rs results) (bad '()))
        (if (null? rs)
          (reverse bad)
          (loop (cdr rs) (if (cadr (car rs)) bad (cons (car (car rs)) bad)))
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
