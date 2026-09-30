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
;; 消息），照搬 gf test -j 已验证的 worker pool + 双 channel 模式：
;;   1. 任务/结果通道容量均为文件数（发送不阻塞）；
;;   2. 全部任务入队后关闭任务通道（close 广播，eof 为无任务哨兵）；
;;   3. 启动 min(jobs, 文件数) 个 worker；
;;   4. 主线程收满结果，按文件序回调打印（乱序到达、按序输出）。
;; 语言之间保持串行（由主入口的逐语言循环保证），本模块只做语言内并行。
;; 并发度参数（-j/--jobs 的解析结果）也寄居于此：所有消费方（主入口与三个
;; 语言的批量层）都已 import 本模块。

(define-library (liii goldfmt-parallel)
  (import (liii base) (liii go) (liii list))
  (export parallel-for-each-ordered serial-for-each-ordered run-worker-loop
    offenders-from count-status set-fmt-jobs! fmt-jobs
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
    ;; 在 worker 会话内循环取任务、调单位函数、回发结果，读到 eof 后退出。
    ;; unit-fn: (lambda (file) ...) 返回 (status msg) 二元表；
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
            (let ((r
                    (catch #t
                      (lambda () (unit-fn (cadr task)))
                      (lambda (tag info) (err-fn tag info))
                    ) ;catch
                  ) ;r
                 ) ;
              (chan-send! result-ch (list (car task) (car r) (cadr r)))
              (loop)
            ) ;let
          ) ;unless
        ) ;let
      ) ;let
    ) ;define

    ;; ---- 串行孪生 -------------------------------------------------------
    ;; 与并行驱动同形的串行实现：同一单位函数、同一回调、同一返回值形状，
    ;; 供 -j 1 / 单文件等场景复用批量层其余部分（打印与统计语义一致）。
    (define (serial-for-each-ordered unit-fn files on-result)
      (let loop
        ((fs files) (acc '()))
        (if (null? fs)
          (reverse acc)
          (let ((r (unit-fn (car fs))))
            (on-result (car fs) r)
            (loop (cdr fs) (cons r acc))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 并行驱动 -------------------------------------------------------
    ;; worker-fn: 各语言导出的 (lambda (worker-args ... task-ch result-ch))，
    ;;   经 (liii go) 派发到独立 worker 会话（内部用 %go-call 按可选前置参数
    ;;   动态构造调用——go 宏要求直接调用语法，无法在驱动内按语言拼装）。
    ;;   worker 从 task-ch 取 (list idx file)，处理完向 result-ch 发
    ;;   (list idx status msg)，读到 eof 后退出。
    ;; worker-args: 该语言 worker 的前置初始化实参（如 cpp 的 clang-format
    ;;   二进制路径字符串），spawn 时一次性序列化传入。
    ;; on-result: (lambda (file result))，result 为 (status msg)，由主线程严格
    ;;   按 files 顺序调用，保证输出与串行一致。
    ;; 返回按 files 顺序的 ((status msg) ...) 列表。
    (define (parallel-for-each-ordered worker-fn files on-result . worker-args)
      (let* ((total (length files))
             (file-vec (list->vector files))
             (task-ch (make-chan total))
             (result-ch (make-chan total))
             (jobs (min (fmt-jobs) total))
             (buf (make-vector total #f))
            ) ;
        ;; 1. 全部任务入队（带序号），然后关闭任务通道。
        (let enq
          ((i 0))
          (when (< i total)
            (chan-send! task-ch (list i (vector-ref file-vec i)))
            (enq (+ i 1))
          ) ;when
        ) ;let
        (chan-close! task-ch)
        ;; 2. 启动 jobs 个 worker（实际并发度 = min(jobs, 文件数)）。
        (let spawn
          ((i 0))
          (when (< i jobs)
            (apply %go-call worker-fn (append worker-args (list task-ch result-ch)))
            (spawn (+ i 1))
          ) ;when
        ) ;let
        ;; 3. 收满 total 个结果：vector 缓冲 + 前向指针按文件序回调 on-result
        ;;    （头部结果就绪即回调，保持日志流式性）。
        (let recv
          ((got 0) (next 0))
          (if (= got total)
            (vector->list buf)
            (let ((r (chan-recv! result-ch)))
              (vector-set! buf (car r) (cdr r))
              (let drain
                ((n next))
                (let ((hit (and (< n total) (vector-ref buf n))))
                  (if (not hit)
                    (recv (+ got 1) n)
                    (begin
                      (on-result (vector-ref file-vec n) hit)
                      (drain (+ n 1))
                    ) ;begin
                  ) ;if
                ) ;let
              ) ;let
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    ;; ---- 结果归约辅助 ---------------------------------------------------
    ;; 统计结果列表中某状态的数量（results 为驱动返回的 (status msg) 列表）。
    (define (count-status sym results)
      (count (lambda (r) (eq? (car r) sym)) results)
    ) ;define

    ;; 由 check 的按序结果列表导出未格式化文件列表（保持文件顺序）。
    ;; results 为驱动返回的 ((ok msg) ...) 列表，fs 为对应的文件列表（等长）。
    ;; ok（每个结果对的首元素）为 #f 的文件进入 offenders。
    (define (offenders-from results fs)
      (let loop
        ((fsl fs) (rs results) (bad '()))
        (if (null? fsl)
          (reverse bad)
          (loop (cdr fsl) (cdr rs) (if (caar rs) bad (cons (car fsl) bad)))
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
