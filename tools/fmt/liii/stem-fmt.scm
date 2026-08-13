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
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; TeXmacs stem 语言处理器：(liii stem-fmt)。
;; .stem 文件是 TeXmacs 宏包的源格式，其中的 quote/quasiquote/unquote/
;; unquote-splicing 是普通符号而非 Scheme reader 语法，格式化时必须保持原样，
;; 不能糖化为 ' ` , ,@（TeXmacs 无法识别这些写法，改写会损坏文件）。
;; 格式化核心复用 (liii goldfmt-scan) / (liii goldfmt-format)，
;; 通过 call-with-stem-mode 动态打开 stem 模式（结构原样约定：
;; 源码中的 'x / ,x 统一输出为 (quote x) / (unquote x)）。
;; 加载时通过 register-lang! 把自己注册进 (liii goldfmt-lang)。

(define-library (liii stem-fmt)
  (import (liii base)
    (liii path)
    (liii string)
    (liii goldfmt-cache)
    (liii goldfmt-scan)
    (liii goldfmt-format)
    (liii goldfmt-lang)
    (liii goldfmt-config)
  ) ;import
  (export stem-extensions)
  (begin

    ;; stem 语言接管的后缀表（带点）。gf_fmt.json 未写 stem.suffix 时也用此表。
    (define stem-extensions '(".stem"))

    ;; scan / format 都必须在 stem 模式下进行，两个包装函数统一入口。
    (define (stem-scan-file path-str)
      (call-with-stem-mode (lambda () (scan-file path-str)))
    ) ;define

    (define (stem-format-nodes nodes)
      (call-with-stem-mode (lambda () (format-nodes nodes)))
    ) ;define

    ;; ---- 单文件格式化 ---------------------------------------------------
    ;; dry-run 模式：输出到终端，不写回。
    (define (format-file-dry-run path-str)
      (let ((formatted (stem-format-nodes (stem-scan-file path-str))))
        (display formatted)
      ) ;let
    ) ;define

    ;; 覆盖原文件。返回 'cached / #t(有变更) / #f(无变更)。
    (define* (format-file path-str (use-cache? #t))
      (if (and use-cache? (fmt-cache-hit? path-str))
        'cached
        (let* ((p (path path-str))
               (original-content (path-read-text p))
               (formatted (stem-format-nodes (stem-scan-file path-str)))
              ) ;
          (if (string=? original-content formatted)
            (begin
              (when use-cache?
                (fmt-cache-touch path-str)
              ) ;when
              #f
            ) ;begin
            (begin
              (path-write-text p formatted)
              (when use-cache?
                (fmt-cache-touch path-str)
              ) ;when
              #t
            ) ;begin
          ) ;if
        ) ;let*
      ) ;if
    ) ;define*

    ;; ---- 文件列表批量格式化 --------------------------------------------
    ;; 返回 (values total updated cached)。
    (define (format-file-list files dry-run excludes)
      (let loop
        ((remaining files) (total 0) (updated 0) (cached 0))
        (if (null? remaining)
          (values total updated cached)
          (let ((file (car remaining)))
            (if (file-excluded? file excludes)
              (loop (cdr remaining) total updated cached)
              (if dry-run
                (begin
                  (display (string-append "Formatting: " file))
                  (newline)
                  (format-file-dry-run file)
                  (loop (cdr remaining) (+ total 1) updated cached)
                ) ;begin
                (let ((result (format-file file)))
                  (cond ((eq? result 'cached) (loop (cdr remaining) (+ total 1) updated (+ cached 1)))
                        (result (display (string-append "  Updated: " file))
                          (newline)
                          (loop (cdr remaining) (+ total 1) (+ updated 1) cached)
                        ) ;result
                        (else (display (string-append "Formatting: " file))
                          (newline)
                          (loop (cdr remaining) (+ total 1) updated cached)
                        ) ;else
                  ) ;cond
                ) ;let
              ) ;if
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (file-extension-match? filename extensions)
      (let loop
        ((exts extensions))
        (if (null? exts)
          #f
          (if (string-ends? filename (car exts)) #t (loop (cdr exts)))
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
          (let ((result (format-file path-str)))
            (cond ((eq? result 'cached) #f)
                  (result (display (string-append "  Updated: " path-str)) (newline))
                  (else (display (string-append "Formatting: " path-str)) (newline))
            ) ;cond
            (display (string-append "Total files formatted: 1, Files updated: "
                       (if (eq? result #t) "1" "0")
                       ", Files cached: "
                       (if (eq? result 'cached) "1" "0")
                     ) ;string-append
            ) ;display
            (newline)
            #t
          ) ;let
        ) ;if
      ) ;if
    ) ;define

    ;; ---- 目录递归格式化 ------------------------------------------------
    ;; 返回 (values total updated cached)。dry-run 不支持目录（保持原约定）。
    (define (format-directory dir-path extensions excludes dry-run)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项仅支持单个文件")
          (newline)
          (exit 1)
        ) ;begin
        (let ((entries (path-list-path (path dir-path))))
          (let loop
            ((i 0) (total 0) (updated 0) (cached 0))
            (if (>= i (vector-length entries))
              (values total updated cached)
              (let ((entry (vector-ref entries i)))
                (cond ((path-file? entry)
                       (let ((entry-str (path->string entry)))
                         (if (and (file-extension-match? entry-str extensions)
                               (not (file-excluded? entry-str excludes))
                             ) ;and
                           (let ((result (format-file entry-str)))
                             (cond ((eq? result 'cached) (loop (+ i 1) (+ total 1) updated (+ cached 1)))
                                   (result (display (string-append "  Updated: " entry-str))
                                     (newline)
                                     (loop (+ i 1) (+ total 1) (+ updated 1) cached)
                                   ) ;result
                                   (else (display (string-append "Formatting: " entry-str))
                                     (newline)
                                     (loop (+ i 1) (+ total 1) updated cached)
                                   ) ;else
                             ) ;cond
                           ) ;let
                           (loop (+ i 1) total updated cached)
                         ) ;if
                       ) ;let
                      ) ;
                      ((path-dir? entry)
                       (let ((dir-str (path->string entry)))
                         (if (file-excluded? dir-str excludes)
                           (loop (+ i 1) total updated cached)
                           (call-with-values (lambda () (format-directory dir-str extensions excludes dry-run))
                             (lambda (sub-total sub-updated sub-cached)
                               (loop (+ i 1) (+ total sub-total) (+ updated sub-updated) (+ cached sub-cached))
                             ) ;lambda
                           ) ;call-with-values
                         ) ;if
                       ) ;let
                      ) ;
                      (else (loop (+ i 1) total updated cached))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;let
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

    ;; 仓库批量格式化：dry-run 恒为 #f（写回），返回 (total updated cached) 列表。
    (define (stem-format-files files cfg)
      (call-with-values (lambda () (format-file-list files #f (lang-excludes 'stem cfg)))
        (lambda (total updated cached) (list total updated cached))
      ) ;call-with-values
    ) ;define

    ;; 单文件 check：scan + format-nodes 与磁盘逐字节比；命中 exclude 视为通过（#t）。
    (define (stem-check-file path-str cfg)
      (let ((excludes (lang-excludes 'stem cfg)))
        (if (file-excluded? path-str excludes)
          #t
          (let ((ondisk (path-read-text (path path-str))))
            (string=? ondisk (stem-format-nodes (stem-scan-file path-str)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 目录格式化（协议适配）：若传入 cfg，以 gf_fmt.json 为准收集并格式化；
    ;; 否则递归格式化指定 dir。dry-run 不支持目录。返回 (total updated cached) 列表。
    (define (stem-format-directory dir extensions excludes dry-run . maybe-cfg)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项仅支持单个文件")
          (newline)
          (exit 1)
        ) ;begin
        (let ((cfg (if (null? maybe-cfg) #f (car maybe-cfg))))
          (if cfg
            (stem-format-files (stem-collect cfg) cfg)
            (call-with-values (lambda () (format-directory dir extensions excludes dry-run))
              (lambda (total updated cached) (list total updated cached))
            ) ;call-with-values
          ) ;if
        ) ;let
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
      ) ;list
    ) ;define

    (register-lang! stem-handler)

  ) ;begin
) ;define-library
