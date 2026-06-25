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
  ) ;import
  (export clang-format-binary
    cpp-extensions
    format-cpp-file
    format-cpp-files
    format-cpp-directory
    check-cpp-file
  ) ;export
  (begin

    ;; C++ 语言接管的后缀表（带点）。gf_fmt.json 未写 cpp.suffix 时也用此表。
    (define cpp-extensions '(".hpp" ".cpp" ".h" ".c" ".cc" ".cxx"))

    ;; ---- clang-format 调用 ----------------------------------------------
    ;; 由用户自行保证 clang-format 已安装并在 PATH 中；这里仅使用通用名。
    ;; 若未找到，给出提示并返回 #f，避免无意义的子进程调用。
    (define (clang-format-binary)
      "clang-format"
    ) ;define

    (define (clang-format-ok?)
      (= 0 (run '(clang-format "--version")))
    ) ;define

    (define (clang-format-hint)
      (display "提示：未找到 clang-format，请安装并确保其在 PATH 中。"
      ) ;display
      (newline)
    ) ;define

    ;; 调用 clang-format。args 为字符串参数列表，opts 传给 run。
    ;; 通过 run-set! 将二进制路径（当前即为通用名）注册到符号命令，
    ;; 使 run 接受列表形式命令，避免拼接 shell 字符串。
    (define (clang-format-run args . opts)
      (let ((sym (string->symbol "clang-format")) (cf (clang-format-binary)))
        (run-set! sym cf)
        (apply run (cons (cons sym args) opts))
      ) ;let
    ) ;define

    ;; 单文件格式化：dry-run 时输出 clang-format 的 dry-run 结果；
    ;; 否则先查缓存，命中则跳过；未命中则用 clang-format -i 原地格式化，
    ;; 通过比较格式化前后内容判断是否有变更。
    ;; 返回 'cached / #t(有变更) / #f(无变更)。
    (define* (format-cpp-file path-str dry-run (use-cache? #t))
      (if (not (clang-format-ok?))
        (begin
          (clang-format-hint)
          #f
        ) ;begin
        (if dry-run
          (clang-format-run (list "--dry-run" path-str))
          (if (and use-cache? (fmt-cache-hit? path-str))
            (begin
              (display (string-append "  Cached: " path-str))
              (newline)
              'cached
            ) ;begin
            (let ((ondisk (path-read-text (path path-str)))
                  (rc (clang-format-run (list "-i" path-str)))
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
    ;; 返回 (total updated cached)。
    (define (format-one-cpp path-str use-cache?)
      (if (and use-cache? (fmt-cache-hit? path-str))
        'cached
        (let ((ondisk (path-read-text (path path-str)))
              (rc (clang-format-run (list "-i" path-str)))
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
                  #f
                ) ;begin
              ) ;if
            ) ;let
            #f
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (format-cpp-files files cfg)
      (if (null? files)
        (begin
          (display "No C++ files found.")
          (newline)
          (list 0 0 0)
        ) ;begin
        (if (not (clang-format-ok?))
          (begin
            (clang-format-hint)
            (list 0 0 0)
          ) ;begin
          (let ((cf (clang-format-binary)))
            (display (string-append "Formatting "
                       (number->string (length files))
                       " C++ files with "
                       cf
                     ) ;string-append
            ) ;display
            (newline)
            (flush-output-port (current-output-port))
            (let loop
              ((fs files) (total 0) (updated 0) (cached 0))
              (if (null? fs)
                (list total updated cached)
                (let ((result (format-one-cpp (car fs) #t)))
                  (cond ((eq? result 'cached) (loop (cdr fs) (+ total 1) updated (+ cached 1)))
                        (result (display (string-append "  Updated: " (car fs)))
                          (newline)
                          (loop (cdr fs) (+ total 1) (+ updated 1) cached)
                        ) ;result
                        (else (loop (cdr fs) (+ total 1) updated cached))
                  ) ;cond
                ) ;let
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;if
    ) ;define

    ;; 目录递归格式化：收集 dir 下命中 suffixes 的 C/C++ 文件（尊重 excludes），
    ;; 逐文件 clang-format 比对内容（同 format-cpp-files）。返回 (total updated unchanged)。
    ;; dry-run 不支持目录（与 scheme 目录约定一致，由调用方拦截）。
    (define (format-cpp-directory dir suffixes excludes)
      (let ((files (collect-files dir suffixes excludes)))
        (if (null? files)
          (begin
            (display "No C++ files found.")
            (newline)
            (list 0 0 0)
          ) ;begin
          (format-cpp-files files #f)
        ) ;if
      ) ;let
    ) ;define

    ;; ---- 单文件检查 -----------------------------------------------------
    ;; 先查缓存，命中则直接通过；未命中再调用 clang-format --dry-run --Werror。
    ;; stdout / stderr 均丢弃；返回 #t(已格式化) / #f(需格式化)。
    ;; 检查通过后 touch 缓存，供后续跳过。
    (define (check-cpp-file path cfg)
      (if (not (clang-format-ok?))
        (begin
          (clang-format-hint)
          #f
        ) ;begin
        (if (fmt-cache-hit? path)
          #t
          (let ((rc (clang-format-run (list "--dry-run" "--Werror" path)
                      :stdout
                      'discard
                      :stderr
                      'discard
                    ) ;clang-format-run
                ) ;rc
               ) ;
            (when (= rc 0)
              (fmt-cache-touch path)
            ) ;when
            (= rc 0)
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

    (define (cpp-format-directory dir exts excludes dry-run)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项仅支持单个文件")
          (newline)
          (exit 1)
        ) ;begin
        (format-cpp-directory dir exts excludes)
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
      ) ;list
    ) ;define

    (register-lang! cpp-handler)

  ) ;begin
) ;define-library
