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
;; 基于 (liii par) 的 vector-par-map（底层为 (liii go) 线程池：每 worker
;; 线程独占独立 s7 解释器会话，channel 传深拷贝消息）做语言内文件级并行：
;; 各语言把"单文件单位函数 + 异常兜底"封装成 file->result 的导出函数交给
;; pool-for-each，无需关心 channel 协议。结果按文件顺序返回（vector-par-map
;; 保序），统计与 offenders 提取均与顺序无关。
;; 语言之间保持串行（由主入口的逐语言循环保证），本模块只做语言内并行。
;; 并发度参数（-j/--jobs 的解析结果）也寄居于此：所有消费方（主入口与三个
;; 语言的批量层）都已 import 本模块。

(define-library (liii goldfmt-parallel)
  (import (liii base) (liii go) (liii list) (liii par))
  (export pool-for-each run-one-task offenders-from count-status
    set-fmt-jobs! fmt-jobs
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

    ;; ---- 单文件任务骨架 ---------------------------------------------------
    ;; 调用单位函数 unit-fn（返回 (status msg) 二元表），异常时由 err-fn
    ;; 兜底构造 (status msg)，统一包装为 (list 路径 状态 失败信息)。
    ;; 供各语言的 file->result 函数引用：这些函数经 vector-par-map ship 到
    ;; worker 会话的只有自身源码，其引用的本骨架与单位函数等符号必须是
    ;; 导出符号（或以内联 lambda 的形式写在其函数体内）。
    (define (run-one-task path unit-fn err-fn)
      (let ((r (catch #t (lambda () (unit-fn path)) (lambda (tag info) (err-fn tag info))))
           ) ;
        (list path (car r) (cadr r))
      ) ;let
    ) ;define

    ;; ---- Worker Pool ----------------------------------------------------
    ;; 基于 (liii par) 的 vector-par-map：file-fn 为各语言导出的
    ;; file->result 函数（返回 (list 路径 状态 失败信息)），分块并发执行，
    ;; 结果按文件顺序返回；on-result 为 (lambda (file result)) 回调，
    ;; 在全部完成后按顺序逐条调用。
    (define (pool-for-each file-fn files on-result)
      (if (null? files)
        '()
        (let ((results
                (vector->list
                  (vector-par-map file-fn (list->vector files) (fmt-jobs))
                ) ;vector->list
              ) ;
             ) ;
          (for-each (lambda (r) (on-result (car r) r)) results)
          results
        ) ;let
      ) ;if
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
