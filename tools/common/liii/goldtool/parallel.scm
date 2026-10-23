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

;; Goldfish 工具共享并发设施：基于 (liii par) 的 worker pool 封装。

(define-library (liii goldtool parallel)
  (import (scheme base) (liii go) (liii list) (liii par))
  (export pool-for-each pool-filter run-one-task offenders-from count-status
    set-goldtool-jobs! goldtool-jobs
  ) ;export
  (begin

    ;; 并发度参数：工具内文件级并发的 worker 数。1 表示串行。
    (define %goldtool-jobs 1)

    ;; 设置并发度：仅接受不小于 1 的数，其余回退 1。
    (define (set-goldtool-jobs! n)
      (set! %goldtool-jobs (if (and (number? n) (>= n 1)) n 1))
    ) ;define

    ;; 读取并发度。
    (define (goldtool-jobs)
      %goldtool-jobs
    ) ;define

    ;; 调用单位函数 unit-fn（返回 (status msg) 二元表），异常时由 err-fn
    ;; 兜底构造 (status msg)，统一包装为 (list 路径 状态 失败信息)。
    (define (run-one-task path unit-fn err-fn)
      (let ((r (catch #t (lambda () (unit-fn path)) (lambda (tag info) (err-fn tag info))))
           ) ;
        (list path (car r) (cadr r))
      ) ;let
    ) ;define

    ;; 基于 (liii par) 的 vector-par-map：file-fn 为各工具导出的
    ;; file->result 函数（返回 (list 路径 状态 失败信息)），分块并发执行，
    ;; 结果按文件顺序返回；on-result 为 (lambda (file result)) 回调，
    ;; 在全部完成后按顺序逐条调用。
    (define (pool-for-each file-fn files on-result)
      (if (null? files)
        '()
        (let ((results (vector->list (vector-par-map file-fn (list->vector files) (goldtool-jobs)))
              ) ;results
             ) ;
          (for-each (lambda (r) (on-result (car r) r)) results)
          results
        ) ;let
      ) ;if
    ) ;define

    ;; 并行过滤：pred 为 (lambda (file) boolean?)，返回使 pred 为 #t 的文件列表。
    (define (pool-filter pred files)
      (if (null? files)
        '()
        (let ((flags (vector->list (vector-par-map pred (list->vector files) (goldtool-jobs)))
              ) ;flags
             ) ;
          (let loop
            ((fs files) (bs flags) (acc '()))
            (if (null? fs)
              (reverse acc)
              (loop (cdr fs) (cdr bs) (if (car bs) (cons (car fs) acc) acc))
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    ;; 统计结果列表中某状态的数量。
    (define (count-status sym results)
      (count (lambda (r) (eq? (cadr r) sym)) results)
    ) ;define

    ;; 由 check 的结果列表提取未通过的文件（ok 为 #f 的路径进入 offenders）。
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
