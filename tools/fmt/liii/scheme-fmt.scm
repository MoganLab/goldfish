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

;; Scheme 语言处理器：(liii scheme-fmt)。
;; 格式化核心复用 (liii goldfmt)，
;; 缓存、单文件/目录/增量格式化逻辑迁移自原 goldfmt.scm。
;; 加载时通过 register-lang! 把自己注册进 (liii goldfmt-lang)。

(define-library (liii scheme-fmt)
  (import (liii base)
    (liii path)
    (liii string)
    (liii list)
    (liii goldfmt-cache)
    (liii goldfmt)
    (liii goldfmt-lang)
    (liii goldfmt-config)
    (liii goldfmt-parallel)
  ) ;import
  (export scheme-extensions format-single-file format-directory format-file-list
    scheme-format-file-quiet scheme-fmt-worker scheme-check-file-quiet
    scheme-check-worker scheme-check-files
  ) ;export
  (begin

    ;; Scheme 语言接管的后缀表（带点）。gf_fmt.json 未写 scheme.suffix 时也用此表。
    (define scheme-extensions '(".scm"))

    ;; ---- 单文件格式化 ---------------------------------------------------
    (define (flush-output)
      (flush-output-port (current-output-port))
    ) ;define

    ;; dry-run 模式：输出到终端，不写回。
    (define (format-file-dry-run path-str)
      (let* ((original-content (path-read-text (path path-str)))
             (err #f)
             (formatted
               (catch #t
                 (lambda () (format-string original-content))
                 (lambda (tag info) (set! err (cons tag info)) #f)
               ) ;catch
             ) ;formatted
            ) ;
        (if err
          (begin
            (print-fmt-failure path-str (fmt-extract-error-message (car err) (cdr err)))
            (exit 1)
          ) ;begin
          (begin
            (display formatted)
            (flush-output)
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    ;; 安静的单位格式化函数：不打印、不 exit（供串行/并行的批量层与
    ;; (liii go) worker 会话共用；worker 只能调用导出符号，故导出）。
    ;; 返回 (list status msg)：status ∈ 'cached / 'updated / 'unchanged / 'failed，
    ;; msg 为失败信息字符串或 #f。
    (define (scheme-format-file-quiet path-str)
      (if (fmt-cache-hit? path-str)
        (list 'cached #f)
        (let* ((p (path path-str))
               (original-content (path-read-text p))
               (err #f)
               (formatted
                 (catch #t
                   (lambda () (format-string original-content))
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
    ;; run-worker-loop / scheme-format-file-quiet / fmt-extract-error-message
    ;; 均为导出符号（worker 会话经 import 可见）。
    (define (scheme-fmt-worker task-ch result-ch)
      (run-worker-loop task-ch result-ch scheme-format-file-quiet
        fmt-extract-error-message
      ) ;run-worker-loop
    ) ;define

    ;; ---- 文件列表批量格式化 --------------------------------------------
    ;; 返回 (values total updated cached failed)。
    ;; 主线程先过滤 exclude；并发度大于 1 且文件数大于 1 时走 (liii go)
    ;; 并行驱动，否则走其串行孪生——两条路径共用同一安静单位函数、打印回调
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
                  (if (and (> (fmt-jobs) 1) (> (length targets) 1))
                    (parallel-for-each-ordered scheme-fmt-worker targets print-result)
                    (serial-for-each-ordered scheme-format-file-quiet targets print-result)
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
          (let* ((result (scheme-format-file-quiet path-str))
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
            (flush-output)
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

    ;; 仓库批量收集：从 cfg 的 scheme.path 递归收集所有 scheme 后缀文件
    ;; （默认 .scm，尊重 gf_fmt.json 的 scheme.suffix 与 scheme.exclude）。
    (define (scheme-collect cfg)
      (let ((paths (lang-paths 'scheme cfg))
            (suffixes (lang-suffixes 'scheme cfg))
            (excludes (lang-excludes 'scheme cfg))
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
    (define (scheme-format-files files cfg)
      (call-with-values (lambda () (format-file-list files #f (lang-excludes 'scheme cfg)))
        (lambda (total updated cached failed) (list total updated cached failed))
      ) ;call-with-values
    ) ;define

    ;; 单文件 check：scan + format-nodes 与磁盘逐字节比；命中 exclude 视为通过（#t）。
    (define (scheme-check-file path-str cfg)
      (let ((excludes (lang-excludes 'scheme cfg)))
        (if (file-excluded? path-str excludes) #t (scheme-check-file-quiet path-str))
      ) ;let
    ) ;define

    ;; 安静的单文件 check（不含 exclude 过滤，批量收集时已过滤；供串行/并行的
    ;; 批量层与 (liii go) worker 会话共用）：与磁盘逐字节比，返回 #t/#f。
    (define (scheme-check-file-quiet path-str)
      (let* ((ondisk (path-read-text (path path-str)))
             (formatted (catch #t (lambda () (format-string ondisk)) (lambda (tag info) #f)))
            ) ;
        (and formatted (string=? ondisk formatted))
      ) ;let*
    ) ;define

    ;; check 的 (liii go) worker：结果包装为 (list ok #f)，异常兜底视为未格式化。
    (define (scheme-check-worker task-ch result-ch)
      (run-worker-loop task-ch
        result-ch
        (lambda (path) (list (scheme-check-file-quiet path) #f))
        (lambda (tag info) (list #f #f))
      ) ;run-worker-loop
    ) ;define

    ;; 批量 check：并发度大于 1 且文件数大于 1 时并行，否则串行；
    ;; 返回未格式化文件列表（保持文件顺序，与串行一致）。
    (define (scheme-check-files files cfg)
      (offenders-from
        (if (and (> (fmt-jobs) 1) (> (length files) 1))
          (parallel-for-each-ordered scheme-check-worker files (lambda (file result) #f))
          (serial-for-each-ordered (lambda (path) (list (scheme-check-file-quiet path) #f))
            files
            (lambda (file result) #f)
          ) ;serial-for-each-ordered
        ) ;if
        files
      ) ;offenders-from
    ) ;define

    ;; 目录格式化（协议适配）：以指定 dir 为准递归格式化。
    ;; 若传入 cfg，合并其 scheme.exclude 配置。dry-run 不支持目录。返回 (total updated cached failed) 列表。
    (define (scheme-format-directory dir extensions excludes dry-run . maybe-cfg)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项仅支持单个文件")
          (newline)
          (exit 1)
        ) ;begin
        (let* ((cfg (if (null? maybe-cfg) #f (car maybe-cfg)))
               (cfg-excludes (if cfg (lang-excludes 'scheme cfg) '()))
               (all-excludes (append excludes cfg-excludes))
              ) ;
          (call-with-values (lambda () (format-directory dir extensions all-excludes dry-run))
            (lambda (total updated cached failed) (list total updated cached failed))
          ) ;call-with-values
        ) ;let*
      ) ;if
    ) ;define

    ;; 注册到语言注册表。
    (define scheme-handler
      (list (cons 'name 'scheme)
        (cons 'label "Scheme")
        (cons 'extensions scheme-extensions)
        (cons 'collect scheme-collect)
        (cons 'format-files scheme-format-files)
        (cons 'format-file format-single-file)
        (cons 'format-directory scheme-format-directory)
        (cons 'check-file scheme-check-file)
        (cons 'check-files scheme-check-files)
      ) ;list
    ) ;define

    (register-lang! scheme-handler)

  ) ;begin
) ;define-library
