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
    (liii goldfmt-cache)
    (liii goldfmt)
    (liii goldfmt-lang)
    (liii goldfmt-config)
  ) ;import
  (export scheme-extensions format-single-file format-directory format-file-list)
  (begin

    ;; Scheme 语言接管的后缀表（带点）。gf_fmt.json 未写 scheme.suffix 时也用此表。
    (define scheme-extensions '(".scm"))

    (define (extract-error-message tag info)
      (let ((raw-msg (cond ((and (pair? info) (string? (car info)))
                            (car info))
                           ((string? tag) tag)
                           ((symbol? tag) (symbol->string tag))
                           (else (object->string tag #f)))))
        (cond ((string-starts? raw-msg "unexpected close paren")
               "unexpected close paren")
              ((string-starts? raw-msg "missing close paren")
               "missing close paren")
              (else
               (string-trim-right raw-msg (lambda (c) (or (char=? c #\:) (char=? c #\space) (char=? c #\newline))))))))

    (define (paren-error? msg)
      (or (string-contains? msg "unexpected close paren")
          (string-contains? msg "missing close paren")))

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
                 (lambda (tag info)
                   (set! err (cons tag info))
                   #f))))
        (if err
          (let ((msg (extract-error-message (car err) (cdr err))))
            (display (string-append "  Failed: " path-str ": " msg))
            (newline)
            (when (paren-error? msg)
              (display (string-append "Hint: try `gf fix " path-str "` to repair common parenthesis issues."))
              (newline))
            (flush-output)
            (exit 1))
          (begin
            (display formatted)
            (flush-output)))))

    ;; 覆盖原文件。返回 'cached / #t(有变更) / #f(无变更) / 'failed。
    (define* (format-file path-str (use-cache? #t))
      (if (and use-cache? (fmt-cache-hit? path-str))
        'cached
        (let* ((p (path path-str))
               (original-content (path-read-text p))
               (err #f)
               (formatted
                 (catch #t
                   (lambda () (format-string original-content))
                   (lambda (tag info)
                     (set! err (cons tag info))
                     #f))))
          (if err
            (let ((msg (extract-error-message (car err) (cdr err))))
              (display (string-append "  Failed: " path-str ": " msg))
              (newline)
              (when (paren-error? msg)
                (display (string-append "Hint: try `gf fix " path-str "` to repair common parenthesis issues."))
                (newline))
              (flush-output)
              'failed)
            (if (string=? original-content formatted)
              (begin
                (when use-cache?
                  (fmt-cache-touch path-str))
                #f)
              (begin
                (path-write-text p formatted)
                (when use-cache?
                  (fmt-cache-touch path-str))
                #t))))))

    ;; ---- 文件列表批量格式化 --------------------------------------------
    ;; 返回 (values total updated cached failed)。
    (define (format-file-list files dry-run excludes)
      (let loop
        ((remaining files) (total 0) (updated 0) (cached 0) (failed 0))
        (if (null? remaining)
          (values total updated cached failed)
          (let ((file (car remaining)))
            (if (file-excluded? file excludes)
              (loop (cdr remaining) total updated cached failed)
              (if dry-run
                (begin
                  (display (string-append "Formatting: " file))
                  (newline)
                  (format-file-dry-run file)
                  (loop (cdr remaining) (+ total 1) updated cached failed)
                ) ;begin
                (let ((result (format-file file)))
                  (cond ((eq? result 'cached) (loop (cdr remaining) (+ total 1) updated (+ cached 1) failed))
                        ((eq? result 'failed) (loop (cdr remaining) (+ total 1) updated cached (+ failed 1)))
                        (result (display (string-append "  Updated: " file))
                          (newline)
                          (loop (cdr remaining) (+ total 1) (+ updated 1) cached failed)
                        ) ;result
                        (else (display (string-append "Formatting: " file))
                          (newline)
                          (loop (cdr remaining) (+ total 1) updated cached failed)
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
                  ((eq? result 'failed)
                   (flush-output)
                   (exit 1))
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
            (flush-output)
            #t
          ) ;let
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
        (let ((entries (path-list-path (path dir-path))))
          (let loop
            ((i 0) (total 0) (updated 0) (cached 0) (failed 0))
            (if (>= i (vector-length entries))
              (values total updated cached failed)
              (let ((entry (vector-ref entries i)))
                (cond
                 ((path-file? entry)
                  (let ((entry-str (path->string entry)))
                    (if (and (file-extension-match? entry-str extensions)
                          (not (file-excluded? entry-str excludes))
                        ) ;and
                      (let ((result (format-file entry-str)))
                        (cond ((eq? result 'cached) (loop (+ i 1) (+ total 1) updated (+ cached 1) failed))
                              ((eq? result 'failed) (loop (+ i 1) (+ total 1) updated cached (+ failed 1)))
                              (result (display (string-append "  Updated: " entry-str))
                                (newline)
                                (loop (+ i 1) (+ total 1) (+ updated 1) cached failed)
                              ) ;result
                              (else (display (string-append "Formatting: " entry-str))
                                (newline)
                                (loop (+ i 1) (+ total 1) updated cached failed)
                              ) ;else
                        ) ;cond
                      ) ;let
                      (loop (+ i 1) total updated cached failed)
                    ) ;if
                  ) ;let
                 ) ;
                 ((path-dir? entry)
                  (let ((dir-str (path->string entry)))
                    (if (file-excluded? dir-str excludes)
                      (loop (+ i 1) total updated cached failed)
                      (call-with-values (lambda () (format-directory dir-str extensions excludes dry-run))
                        (lambda (sub-total sub-updated sub-cached sub-failed)
                          (loop (+ i 1) (+ total sub-total) (+ updated sub-updated) (+ cached sub-cached) (+ failed sub-failed))
                        ) ;lambda
                      ) ;call-with-values
                    ) ;if
                  ) ;let
                 ) ;
                 (else (loop (+ i 1) total updated cached failed))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;let
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
        (if (file-excluded? path-str excludes)
          #t
          (let* ((ondisk (path-read-text (path path-str)))
                 (formatted
                   (catch #t
                     (lambda () (format-string ondisk))
                     (lambda (tag info) #f))))
            (and formatted (string=? ondisk formatted))
          ) ;let*
        ) ;if
      ) ;let
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
      ) ;list
    ) ;define

    (register-lang! scheme-handler)

  ) ;begin
) ;define-library
