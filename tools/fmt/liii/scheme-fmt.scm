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
;; 格式化核心复用 (liii goldfmt-scan) / (liii goldfmt-format)，
;; 缓存、单文件/目录/增量格式化逻辑迁移自原 goldfmt.scm。
;; 加载时通过 register-lang! 把自己注册进 (liii goldfmt-lang)。

(define-library (liii scheme-fmt)
  (import (liii base)
    (liii path)
    (liii string)
    (liii hashlib)
    (liii goldfmt-scan)
    (liii goldfmt-format)
    (liii goldfmt-lang)
    (liii goldtool-changed)
  ) ;import
  (export format-single-file
    format-directory
    format-changed-since
    format-file-list
    collect-scheme
    check-scheme-file
    scheme-handler
  ) ;export
  (begin

    ;; ---- 缓存（迁移自 goldfmt.scm）-------------------------------------
    (define (fmt-cache-base-dir)
      (path->string (path-join (path-home) ".cache" "goldfish" "fmt" (version)))
    ) ;define

    (define (fmt-cache-path file-path)
      (let* ((hash (sha256-by-file file-path))
             (prefix (substring hash 0 2))
             (rest (substring hash 2))
             (base (fmt-cache-base-dir))
            ) ;
        (path->string (path-join base prefix rest))
      ) ;let*
    ) ;define

    (define (fmt-cache-hit? file-path)
      (let ((cache (fmt-cache-path file-path)))
        (file-exists? cache)
      ) ;let
    ) ;define

    (define (fmt-cache-touch file-path)
      (let* ((cache (fmt-cache-path file-path))
             (cache-dir (path->string (path-parent (path cache))))
            ) ;
        (unless (path-dir? (path cache-dir))
          (g_mkdir cache-dir)
        ) ;unless
        (path-touch (path cache))
      ) ;let*
    ) ;define

    ;; ---- 单文件格式化 ---------------------------------------------------
    ;; dry-run 模式：输出到终端，不写回。
    (define (format-file-dry-run path-str)
      (let* ((nodes (scan-file path-str)) (formatted (format-nodes nodes)))
        (display formatted)
      ) ;let*
    ) ;define

    ;; 覆盖原文件。返回 'cached / #t(有变更) / #f(无变更)。
    (define* (format-file path-str (use-cache? #t))
      (if (and use-cache? (fmt-cache-hit? path-str))
        'cached
        (let* ((p (path path-str))
               (original-content (path-read-text p))
               (nodes (scan-file path-str))
               (formatted (format-nodes nodes))
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

    ;; ---- 增量格式化 -----------------------------------------------------
    (define (format-changed-since since path-str extensions excludes dry-run)
      (let ((scope (if (string=? path-str "") #f path-str)))
        (let ((files (if scope
                       (changed-scheme-files-since since scope extensions)
                       (changed-scheme-files-since since #f extensions)
                     ) ;if
              ) ;files
             ) ;
          (if (null? files)
            (begin
              (display (string-append "No changed Scheme files since " since))
              (newline)
              #t
            ) ;begin
            (call-with-values (lambda () (format-file-list files dry-run excludes))
              (lambda (total updated cached)
                (display (string-append "Total files formatted: "
                           (number->string total)
                           ", Files updated: "
                           (number->string updated)
                           ", Files cached: "
                           (number->string cached)
                         ) ;string-append
                ) ;display
                (newline)
                #t
              ) ;lambda
            ) ;call-with-values
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; ---- handler 协议实现（供仓库批量 / check 使用）---------------------
    ;; 按配置 path 收集所有 .scm 文件（suffixes 由调用方传，默认 .scm）。
    (define (collect-scheme paths excludes)
      (let loop
        ((ps paths) (acc '()))
        (if (null? ps)
          acc
          (if (path-dir? (path (car ps)))
            (loop (cdr ps) (append (collect-files (car ps) '(".scm") excludes) acc))
            (loop (cdr ps) acc)
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    ;; 逐文件 check：gf fmt --dry-run 输出与磁盘内容逐字节比较。
    ;; 命中 exclude 或命令失败视为通过（#t）。
    (define (check-scheme-file path-str excludes)
      (if (file-excluded? path-str excludes)
        #t
        (let ((nodes (scan-file path-str)) (ondisk (path-read-text (path path-str))))
          (string=? ondisk (format-nodes nodes))
        ) ;let
      ) ;if
    ) ;define

    ;; 注册到语言注册表。
    (define (scheme-check-file path excludes)
      (check-scheme-file path excludes)
    ) ;define

    (define (scheme-format-files files dry-run excludes)
      (format-file-list files dry-run excludes)
    ) ;define

    (define scheme-handler
      (list (cons 'name 'scheme)
        (cons 'collect collect-scheme)
        (cons 'check-file scheme-check-file)
        (cons 'format-files scheme-format-files)
      ) ;list
    ) ;define

    (register-lang! scheme-handler)

  ) ;begin
) ;define-library
