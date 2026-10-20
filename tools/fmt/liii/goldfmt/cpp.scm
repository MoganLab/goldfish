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

(define-library (liii goldfmt cpp)
  (import (liii base)
    (liii path)
    (liii string)
    (liii goldfmt cache)
    (liii goldfmt lang)
    (liii goldfmt config)
    (liii goldfmt parallel)
    (liii goldfmt cpp-runner)
  ) ;import
  (export clang-format-binary cpp-extensions format-cpp-file format-cpp-files
    format-cpp-directory check-cpp-file cpp-format-one-quiet cpp-fmt-one
    cpp-check-file-quiet cpp-check-one cpp-check-files
  ) ;export
  (begin

    ;; C++ 语言接管的后缀表（带点）。gf_fmt.json 未写 cpp.suffix 时也用此表。
    (define cpp-extensions '(".hpp" ".cpp" ".h" ".c" ".cc" ".cxx"))

    ;; 单文件格式化：dry-run 时输出 clang-format 的 dry-run 结果；
    ;; 否则先查缓存，命中则跳过；未命中则用 clang-format -i 原地格式化，
    ;; 通过比较格式化前后内容判断是否有变更。
    ;; 返回 'cached / #t(有变更) / #f(无变更)。
    (define* (format-cpp-file path-str dry-run (use-cache? #t))
      (let ((cfg (load-fmt-config)))
        (cond ((not (clang-format-ok? cfg)) (clang-format-hint) #f)
              (dry-run (clang-format-run cfg (list "--dry-run" path-str)))
              ((and use-cache? (fmt-cache-hit? path-str))
               (display (string-append "  Cached: " path-str))
               (newline)
               'cached
              ) ;
              (else
                (let* ((res (cpp-format-one-quiet (clang-format-binary cfg) path-str))
                       (status (car res))
                      ) ;
                  (cond ((eq? status 'updated)
                         (display (string-append "  Updated: " path-str))
                         (newline)
                         #t
                        ) ;
                        ((eq? status 'cached)
                         (display (string-append "  Cached: " path-str))
                         (newline)
                         'cached
                        ) ;
                        ((eq? status 'unchanged)
                         (display (string-append "  Unchanged: " path-str))
                         (newline)
                         #f
                        ) ;
                        (else (display (string-append "  Failed: " path-str)) (newline) #f)
                  ) ;cond
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define*

    ;; ---- 文件收集 -------------------------------------------------------
    ;; 仓库批量收集：从 cfg 的 cpp.path 递归收集 C/C++ 文件（按 cpp.suffix，尊重 cpp.exclude）。
    (define (cpp-collect cfg)
      (let ((paths (lang-paths 'cpp cfg))
            (suffixes (lang-suffixes 'cpp cfg))
            (excludes (lang-excludes 'cpp cfg))
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

    ;; ---- 批量格式化 -----------------------------------------------------
    ;; 逐文件 clang-format：先查缓存，命中则跳过；未命中则用 clang-format -i
    ;; 原地格式化，通过比较格式化前后内容判断是否有变更。
    ;; 返回 (total updated cached)。failed 与 unchanged 均不计数（保持原语义）。
    (define (format-cpp-files files cfg)
      (if (null? files)
        (begin
          (display "No C++ files found.")
          (newline)
          (list 0 0 0)
        ) ;begin
        (if (not (clang-format-ok? cfg))
          (begin
            (clang-format-hint)
            (list 0 0 0)
          ) ;begin
          (let ((cf (clang-format-binary cfg)))
            (display (string-append "Formatting "
                       (number->string (length files))
                       " C++ files with "
                       cf
                     ) ;string-append
            ) ;display
            (newline)
            (flush-output-port (current-output-port))
            (let ((results (pool-for-each (lambda (path) (cpp-fmt-one cf path)) files print-cpp-fmt-result)
                  ) ;results
                 ) ;
              (list (length results)
                (count-status 'updated results)
                (count-status 'cached results)
              ) ;list
            ) ;let
          ) ;let
        ) ;if
      ) ;if
    ) ;define

    ;; 目录递归格式化：在 dir 下收集命中 suffixes 的 C/C++ 文件（尊重 excludes 与 cfg 排除）。
    ;; 逐文件 clang-format 比对内容。返回 (total updated unchanged)。dry-run 不支持目录（由调用方拦截）。
    (define (format-cpp-directory dir suffixes excludes . maybe-cfg)
      (let* ((cfg (if (null? maybe-cfg) #f (car maybe-cfg)))
             (cfg-excludes (if cfg (lang-excludes 'cpp cfg) '()))
             (all-excludes (append excludes cfg-excludes))
             (files (collect-files dir suffixes all-excludes))
            ) ;
        (if (null? files)
          (begin
            (display "No C++ files found.")
            (newline)
            (list 0 0 0)
          ) ;begin
          (format-cpp-files files cfg)
        ) ;if
      ) ;let*
    ) ;define

    ;; ---- 单文件检查 -----------------------------------------------------
    ;; 先查缓存，命中则直接通过；未命中再调用 clang-format --dry-run --Werror。
    ;; stdout / stderr 均丢弃；返回 #t(已格式化) / #f(需格式化)。
    ;; 检查通过后 touch 缓存，供后续跳过。
    (define (check-cpp-file path cfg)
      (if (not (clang-format-ok? cfg))
        (begin
          (clang-format-hint)
          #f
        ) ;begin
        (cpp-check-file-quiet (clang-format-binary cfg) path)
      ) ;if
    ) ;define

    ;; 批量 check：可用性探测上提到批量层（不可用时提示只打印一次、全部文件
    ;; 视为未格式化——与原逐文件探测的 offenders 结果一致，仅提示次数减少）；
    ;; 统一走 Worker Pool（worker 数 = min(jobs, 文件数)）。返回未格式化文件列表（按到达顺序）。
    (define (cpp-check-files files cfg)
      (if (null? files)
        '()
        (if (not (clang-format-ok? cfg))
          (begin
            (clang-format-hint)
            files
          ) ;begin
          (let ((cf (clang-format-binary cfg)))
            (offenders-from (pool-for-each (lambda (path) (cpp-check-one cf path))
                              files
                              (lambda (file result) #f)
                            ) ;pool-for-each
            ) ;offenders-from
          ) ;let
        ) ;if
      ) ;if
    ) ;define

    ;; ---- 注册到语言注册表 -----------------------------------------------
    ;; format-file / format-directory 用 lambda 适配到统一协议签名
    ;; （cpp 单文件不读 excludes；cpp 目录不支持 dry-run，由主入口拦截）。
    (define (cpp-format-file path dry-run excludes)
      (format-cpp-file path dry-run)
    ) ;define

    (define (cpp-format-directory dir exts excludes dry-run . maybe-cfg)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项仅支持单个文件")
          (newline)
          (exit 1)
        ) ;begin
        (apply format-cpp-directory dir exts excludes maybe-cfg)
      ) ;if
    ) ;define

    (define cpp-handler
      (list (cons 'name 'cpp)
        (cons 'label "C++")
        (cons 'extensions cpp-extensions)
        (cons 'collect cpp-collect)
        (cons 'format-files format-cpp-files)
        (cons 'format-file cpp-format-file)
        (cons 'format-directory cpp-format-directory)
        (cons 'check-file check-cpp-file)
        (cons 'check-files cpp-check-files)
      ) ;list
    ) ;define

    (register-lang! cpp-handler)

  ) ;begin
) ;define-library
