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

;; C++ 语言处理器：(liii cpp-fmt)。
;; 格式化通过外部 clang-format 完成（-i 原地改；--dry-run --Werror 做检查）。
;; 文件收集复用 (liii goldfmt-lang) 的 collect-files + glob 排除。
;; 加载时通过 register-lang! 注册进 (liii goldfmt-lang)。

(define-library (liii cpp-fmt)
  (import (liii base)
    (liii sys)
    (liii os)
    (liii path)
    (liii string)
    (liii subprocess)
    (liii goldfmt-cache)
    (liii goldfmt-lang)
    (liii goldfmt-config)
    (liii goldfmt-parallel)
  ) ;import
  (export clang-format-binary cpp-extensions format-cpp-file format-cpp-files
    format-cpp-directory check-cpp-file cpp-format-one-quiet cpp-fmt-worker
    cpp-check-file-quiet cpp-check-worker cpp-check-files
  ) ;export
  (begin

    ;; C++ 语言接管的后缀表（带点）。gf_fmt.json 未写 cpp.suffix 时也用此表。
    (define cpp-extensions '(".hpp" ".cpp" ".h" ".c" ".cc" ".cxx"))

    ;; ---- clang-format 调用 ----------------------------------------------
    ;; 优先从 gf_fmt.json 的 cpp.binary / cpp.binary-linux / cpp.binary-windows /
    ;; cpp.binary-macos 读取；未配置时回退到 PATH 中的 "clang-format"。
    ;; 若未找到，给出提示并返回 #f，避免无意义的子进程调用。
    (define (clang-format-binary . maybe-cfg)
      (let ((cfg (if (null? maybe-cfg) #f (car maybe-cfg))))
        (if cfg (lang-binary 'cpp cfg) "clang-format")
      ) ;let
    ) ;define

    (define (clang-format-ok? . maybe-cfg)
      (let ((cf (apply clang-format-binary maybe-cfg)))
        (let ((sym (string->symbol "clang-format")))
          (run-set! sym cf)
          (= 0 (run '(clang-format "--version")))
        ) ;let
      ) ;let
    ) ;define

    (define (clang-format-hint)
      (display "提示：未找到 clang-format，请安装并确保其在 PATH 中，或在 gf_fmt.json 配置 cpp.binary。"
      ) ;display
      (newline)
    ) ;define

    ;; 调用 clang-format。cfg 为已加载的 gf_fmt.json 配置（可为 #f），args 为字符串参数
    ;; 列表，opts 传给 run。通过 run-set! 将配置得到的路径注册到符号命令。
    (define (clang-format-run cfg args . opts)
      (apply clang-format-run-with (clang-format-binary cfg) args opts)
    ) ;define

    ;; 以指定的 clang-format 二进制路径调用（不依赖 cfg 对象：cfg 是 (liii json) 的
    ;; C 层对象，不可跨 (liii go) 会话序列化，并发改造时由主线程先解析出路径字符串）。
    (define (clang-format-run-with cf args . opts)
      (let ((sym (string->symbol "clang-format")))
        (run-set! sym cf)
        (apply run (cons (cons sym args) opts))
      ) ;let
    ) ;define

    ;; 单文件格式化：dry-run 时输出 clang-format 的 dry-run 结果；
    ;; 否则先查缓存，命中则跳过；未命中则用 clang-format -i 原地格式化，
    ;; 通过比较格式化前后内容判断是否有变更。
    ;; 返回 'cached / #t(有变更) / #f(无变更)。
    (define* (format-cpp-file path-str dry-run (use-cache? #t))
      (let ((cfg (load-fmt-config)))
        (if (not (clang-format-ok? cfg))
          (begin
            (clang-format-hint)
            #f
          ) ;begin
          (if dry-run
            (clang-format-run cfg (list "--dry-run" path-str))
            (if (and use-cache? (fmt-cache-hit? path-str))
              (begin
                (display (string-append "  Cached: " path-str))
                (newline)
                'cached
              ) ;begin
              (let ((ondisk (path-read-text (path path-str)))
                    (rc (clang-format-run cfg (list "-i" path-str)))
                   ) ;
                (if (= rc 0)
                  (let ((formatted (path-read-text (path path-str))))
                    (if (not (string=? formatted ondisk))
                      (begin
                        (when use-cache?
                          (fmt-cache-touch path-str)
                        ) ;when
                        (display (string-append "  Updated: " path-str))
                        (newline)
                        #t
                      ) ;begin
                      (begin
                        (when use-cache?
                          (fmt-cache-touch path-str)
                        ) ;when
                        (display (string-append "  Unchanged: " path-str))
                        (newline)
                        #f
                      ) ;begin
                    ) ;if
                  ) ;let
                  (begin
                    (display (string-append "  Failed: " path-str))
                    (newline)
                    #f
                  ) ;begin
                ) ;if
              ) ;let
            ) ;if
          ) ;if
        ) ;if
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
    ;; 安静的单位格式化函数：不打印、不 exit（供串行/并行的批量层与 (liii go)
    ;; worker 会话共用；cf 为 clang-format 二进制路径字符串，主线程从 cfg 解析）。
    ;; 返回 (list status msg)：status ∈ 'cached / 'updated / 'unchanged / 'failed，
    ;; msg 恒为 #f（保持既有静默失败语义：失败不打印、不单独计数）。
    (define (cpp-format-one-quiet cf path-str)
      (if (fmt-cache-hit? path-str)
        (list 'cached #f)
        (let ((ondisk (path-read-text (path path-str)))
              (rc (clang-format-run-with cf (list "-i" path-str)))
             ) ;
          (if (= rc 0)
            (let ((formatted (path-read-text (path path-str))))
              (if (not (string=? formatted ondisk))
                (begin
                  (fmt-cache-touch path-str)
                  (list 'updated #f)
                ) ;begin
                (begin
                  (fmt-cache-touch path-str)
                  (list 'unchanged #f)
                ) ;begin
              ) ;if
            ) ;let
            (list 'failed #f)
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; (liii go) worker：在独立会话中循环取任务、调安静单位函数、回发结果
    ;; （骨架见 run-worker-loop；异常兜底转 failed 结果）。cf（clang-format
    ;; 二进制路径字符串）由主线程从 cfg 解析后经 spawn 参数一次性传入——
    ;; 直接传解析好的字符串比把 cfg 交给 worker 重解析更省（每次批量
    ;; 只解析一次配置）。
    (define (cpp-fmt-worker cf task-ch result-ch)
      (run-worker-loop task-ch
        result-ch
        (lambda (path) (cpp-format-one-quiet cf path))
        (lambda (tag info) (list 'failed #f))
      ) ;run-worker-loop
    ) ;define

    ;; 主线程按结果打印一个文件的处理行（cpp 只打印 Updated 行，失败静默，
    ;; 保持原语义）。
    (define (print-result file result)
      (when (eq? (cadr result) 'updated)
        (display (string-append "  Updated: " file))
        (newline)
      ) ;when
    ) ;define

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
            (let ((results
                    (if (and (> (fmt-jobs) 1) (> (length files) 1))
                      (pool-for-each cpp-fmt-worker files print-result cf)
                      (serial-for-each (lambda (file) (cpp-format-one-quiet cf file))
                        files
                        print-result
                      ) ;serial-for-each
                    ) ;if
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

    ;; 安静的单文件 check（不探测可用性、不打印提示；cf 为二进制路径字符串，
    ;; 供串行/并行的批量层与 (liii go) worker 会话共用）。
    (define (cpp-check-file-quiet cf path)
      (if (fmt-cache-hit? path)
        #t
        (let ((rc (clang-format-run-with cf
                    (list "--dry-run" "--Werror" path)
                    :stdout 'discard
                    :stderr 'discard
                  ) ;clang-format-run-with
              ) ;rc
             ) ;
          (when (= rc 0)
            (fmt-cache-touch path)
          ) ;when
          (= rc 0)
        ) ;let
      ) ;if
    ) ;define

    ;; check 的 (liii go) worker：cf 经 spawn 参数传入（主线程已预检可用性）；
    ;; 结果包装为 (list ok #f)，异常兜底视为未格式化。
    (define (cpp-check-worker cf task-ch result-ch)
      (run-worker-loop task-ch
        result-ch
        (lambda (path) (list (cpp-check-file-quiet cf path) #f))
        (lambda (tag info) (list #f #f))
      ) ;run-worker-loop
    ) ;define

    ;; 批量 check：可用性探测上提到批量层（不可用时提示只打印一次、全部文件
    ;; 视为未格式化——与原逐文件探测的 offenders 结果一致，仅提示次数减少）；
    ;; 并发度大于 1 且文件数大于 1 时并行，否则串行。返回未格式化文件列表。
    (define (cpp-check-files files cfg)
      (if (null? files)
        '()
        (if (not (clang-format-ok? cfg))
          (begin
            (clang-format-hint)
            files
          ) ;begin
          (let ((cf (clang-format-binary cfg)))
            (offenders-from
              (if (and (> (fmt-jobs) 1) (> (length files) 1))
                (pool-for-each cpp-check-worker files (lambda (file result) #f) cf)
                (serial-for-each (lambda (path) (list (cpp-check-file-quiet cf path) #f))
                  files
                  (lambda (file result) #f)
                ) ;serial-for-each
              ) ;if
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
