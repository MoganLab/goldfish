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

;; TeXmacs stem 语言处理器：(liii stem-fmt)。
;; .stem 文件是 TeXmacs 宏包的源格式，其中的 quote/quasiquote/unquote/
;; unquote-splicing 是普通符号而非 Scheme reader 语法，格式化时必须保持原样，
;; 不能糖化为 ' ` , ,@（TeXmacs 无法识别这些写法，改写会损坏文件）。
;; 格式化核心复用 (liii goldfmt stem)，
;; 通过 format-stem-string 格式化（结构原样约定：
;; 源码中的 'x / ,x 统一输出为 (quote x) / (unquote x)）。
;; 加载时通过 register-lang! 把自己注册进 (liii goldfmt-lang)。

(define-library (liii stem-fmt)
  (import (liii base)
    (liii path)
    (liii string)
    (liii list)
    (liii goldfmt-cache)
    (liii goldfmt stem)
    (liii goldfmt-lang)
    (liii goldfmt-config)
    (liii goldfmt-parallel)
  ) ;import
  (export stem-extensions stem-format-file-quiet stem-fmt-worker
    stem-check-file-quiet stem-check-worker stem-check-files
  ) ;export
  (begin

    ;; stem 语言接管的后缀表（带点）。gf_fmt.json 未写 stem.suffix 时也用此表。
    (define stem-extensions '(".stem"))

    ;; ---- 单文件格式化 ---------------------------------------------------
    ;; dry-run 模式：输出到终端，不写回。
    (define (format-file-dry-run path-str)
      (let* ((p (path path-str))
             (original-content (path-read-text p))
             (err #f)
             (formatted
               (catch #t
                 (lambda () (format-stem-string original-content))
                 (lambda (tag info) (set! err (cons tag info)) #f)
               ) ;catch
             ) ;formatted
            ) ;
        (if err
          (begin
            (print-fmt-failure path-str (fmt-extract-error-message (car err) (cdr err)))
            (exit 1)
          ) ;begin
          (display formatted)
        ) ;if
      ) ;let*
    ) ;define

    ;; 安静的单位格式化函数：不打印、不 exit（供串行/并行的批量层与
    ;; (liii go) worker 会话共用）。返回 (list status msg)：
    ;; status ∈ 'cached / 'updated / 'unchanged / 'failed，msg 为字符串或 #f。
    (define (stem-format-file-quiet path-str)
      (if (fmt-cache-hit? path-str)
        (list 'cached #f)
        (let* ((p (path path-str))
               (original-content (path-read-text p))
               (err #f)
               (formatted
                 (catch #t
                   (lambda () (format-stem-string original-content))
                   (lambda (tag info) (set! err (cons tag info)) #f)
                 ) ;catch
               ) ;formatted
              ) ;
          (if err
            (list 'failed (fmt-extract-error-message (car err) (cdr err)))
            (if (string=? original-content formatted)
              (begin
                (fmt-cache-touch path-str)
                (list 'unchanged #f)
              ) ;begin
              (begin
                (path-write-text p formatted)
                (fmt-cache-touch path-str)
                (list 'updated #f)
              ) ;begin
            ) ;if
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; 主线程按结果打印一个文件的处理行。
    (define (print-result file result)
      (print-fmt-result-line file (car result) (cadr result))
    ) ;define

    ;; (liii go) worker：在独立会话中循环取任务、调安静单位函数、回发结果
    ;; （骨架见 run-worker-loop；异常兜底转 failed 结果，避免主线程收不齐
    ;; 结果而永久等待）。(go ...) 只 ship 本函数自身源码，其引用的
    ;; run-worker-loop / stem-format-file-quiet / fmt-extract-error-message
    ;; 均为导出符号（worker 会话经 import 可见）。
    (define (stem-fmt-worker task-ch result-ch)
      (run-worker-loop task-ch result-ch stem-format-file-quiet
        fmt-extract-error-message
      ) ;run-worker-loop
    ) ;define

    ;; ---- 文件列表批量格式化 --------------------------------------------
    ;; 返回 (values total updated cached failed)。
    ;; 主线程先过滤 exclude；并发度大于 1 且待实际格式化（缓存未命中）的文件
    ;; 多于 2 个时走 (liii go) 并行驱动，否则走串行孪生——并发有数十毫秒的
    ;; 固定启动开销（线程池 + 每 worker 会话 import 库栈），工作量太小不划算；
    ;; 两条路径共用同一安静单位函数、打印回调
    ;; 与统计归约，输出与统计语义一致（并行结果按文件序回调打印）。
    (define (format-file-list files dry-run excludes)
      (let ((targets (filter (lambda (f) (not (file-excluded? f excludes))) files)))
        (if dry-run
          (let loop
            ((remaining targets) (total 0))
            (if (null? remaining)
              (values total 0 0 0)
              (begin
                (display (string-append "Formatting: " (car remaining)))
                (newline)
                (format-file-dry-run (car remaining))
                (loop (cdr remaining) (+ total 1))
              ) ;begin
            ) ;if
          ) ;let
          (let ((results
                  (if (and (> (fmt-jobs) 1)
                        (> (length targets) 2)
                        (> (fmt-cache-miss-count targets) 2)
                      ) ;and
                    (parallel-for-each-ordered stem-fmt-worker targets print-result)
                    (serial-for-each-ordered stem-format-file-quiet targets print-result)
                  ) ;if
                ) ;results
               ) ;
            (values (length results)
              (count-status 'updated results)
              (count-status 'cached results)
              (count-status 'failed results)
            ) ;values
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 单文件入口（供主入口有路径参数时调用）-------------------------
    ;; 返回 #t（正常结束）。
    (define (format-single-file path-str dry-run excludes)
      (if (file-excluded? path-str excludes)
        (begin
          (display (string-append "Skipped (excluded): " path-str))
          (newline)
          #t
        ) ;begin
        (if dry-run
          (format-file-dry-run path-str)
          (let* ((result (stem-format-file-quiet path-str))
                 (status (car result))
                 (msg (cadr result))
                ) ;
            (cond ((eq? status 'cached) #f)
                  ((eq? status 'failed) (print-fmt-failure path-str msg) (exit 1))
                  (else (print-fmt-result-line path-str status msg))
            ) ;cond
            (display (string-append "Total files formatted: 1, Files updated: "
                       (if (eq? status 'updated) "1" "0")
                       ", Files cached: "
                       (if (eq? status 'cached) "1" "0")
                     ) ;string-append
            ) ;display
            (newline)
            #t
          ) ;let*
        ) ;if
      ) ;if
    ) ;define

    ;; ---- 目录递归格式化 ------------------------------------------------
    ;; 返回 (values total updated cached failed)。dry-run 不支持目录（保持原约定）。
    (define (format-directory dir-path extensions excludes dry-run)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项仅支持单个文件")
          (newline)
          (exit 1)
        ) ;begin
        (format-file-list (collect-files-dfs dir-path extensions excludes) #f excludes)
      ) ;if
    ) ;define

    ;; ---- handler 协议实现（供仓库批量 / check 使用）---------------------
    ;; 各方法统一接收 cfg，内部用 goldfmt-config 访问器自取本语言的 path/exclude。

    ;; 仓库批量收集：从 cfg 的 stem.path 递归收集所有 stem 后缀文件
    ;; （默认 .stem，尊重 gf_fmt.json 的 stem.suffix 与 stem.exclude）。
    (define (stem-collect cfg)
      (let ((paths (lang-paths 'stem cfg))
            (suffixes (lang-suffixes 'stem cfg))
            (excludes (lang-excludes 'stem cfg))
           ) ;
        (let loop
          ((ps paths) (acc '()))
          (if (null? ps)
            acc
            (if (path-dir? (path (car ps)))
              (loop (cdr ps) (append (collect-files (car ps) suffixes excludes) acc))
              (loop (cdr ps) acc)
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 仓库批量格式化：dry-run 恒为 #f（写回），返回 (total updated cached failed) 列表。
    (define (stem-format-files files cfg)
      (call-with-values (lambda () (format-file-list files #f (lang-excludes 'stem cfg)))
        (lambda (total updated cached failed) (list total updated cached failed))
      ) ;call-with-values
    ) ;define

    ;; 单文件 check：scan + format-nodes 与磁盘逐字节比；命中 exclude 视为通过（#t）。
    (define (stem-check-file path-str cfg)
      (let ((excludes (lang-excludes 'stem cfg)))
        (if (file-excluded? path-str excludes) #t (stem-check-file-quiet path-str))
      ) ;let
    ) ;define

    ;; 安静的单文件 check（不含 exclude 过滤，批量收集时已过滤）：返回 #t/#f。
    (define (stem-check-file-quiet path-str)
      (let* ((ondisk (path-read-text (path path-str)))
             (formatted (catch #t (lambda () (format-stem-string ondisk)) (lambda (tag info) #f))
             ) ;formatted
            ) ;
        (and formatted (string=? ondisk formatted))
      ) ;let*
    ) ;define

    ;; check 的 (liii go) worker：结果包装为 (list ok #f)，异常兜底视为未格式化。
    (define (stem-check-worker task-ch result-ch)
      (run-worker-loop task-ch
        result-ch
        (lambda (path) (list (stem-check-file-quiet path) #f))
        (lambda (tag info) (list #f #f))
      ) ;run-worker-loop
    ) ;define

    ;; 批量 check：并发度大于 1 且文件数多于 2 时并行（check 无缓存可跳过，
    ;; 每个文件都是完整工作量），否则串行；
    ;; 返回未格式化文件列表（保持文件顺序，与串行一致）。
    (define (stem-check-files files cfg)
      (offenders-from
        (if (and (> (fmt-jobs) 1) (> (length files) 2))
          (parallel-for-each-ordered stem-check-worker files (lambda (file result) #f))
          (serial-for-each-ordered (lambda (path) (list (stem-check-file-quiet path) #f))
            files
            (lambda (file result) #f)
          ) ;serial-for-each-ordered
        ) ;if
        files
      ) ;offenders-from
    ) ;define

    ;; 目录格式化（协议适配）：以指定 dir 为准递归收集并格式化。
    ;; 若传入 cfg，合并其 stem.exclude 配置。dry-run 不支持目录。返回 (total updated cached failed) 列表。
    (define (stem-format-directory dir extensions excludes dry-run . maybe-cfg)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项仅支持单个文件")
          (newline)
          (exit 1)
        ) ;begin
        (let* ((cfg (if (null? maybe-cfg) #f (car maybe-cfg)))
               (cfg-excludes (if cfg (lang-excludes 'stem cfg) '()))
               (all-excludes (append excludes cfg-excludes))
              ) ;
          (call-with-values (lambda () (format-directory dir extensions all-excludes dry-run))
            (lambda (total updated cached failed) (list total updated cached failed))
          ) ;call-with-values
        ) ;let*
      ) ;if
    ) ;define

    ;; 注册到语言注册表。
    (define stem-handler
      (list (cons 'name 'stem)
        (cons 'label "TeXmacs Stem")
        (cons 'extensions stem-extensions)
        (cons 'collect stem-collect)
        (cons 'format-files stem-format-files)
        (cons 'format-file format-single-file)
        (cons 'format-directory stem-format-directory)
        (cons 'check-file stem-check-file)
        (cons 'check-files stem-check-files)
      ) ;list
    ) ;define

    (register-lang! stem-handler)

  ) ;begin
) ;define-library
