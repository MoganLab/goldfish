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

(define (%goldfmt-common-dirname path-str)
  (let loop
    ((i (- (string-length path-str) 1)))
    (cond ((< i 0) ".")
          ((or (char=? (string-ref path-str i) #\/) (char=? (string-ref path-str i) #\\))
           (if (= i 0) "." (substring path-str 0 i))
          ) ;
          (else (loop (- i 1)))
    ) ;cond
  ) ;let
) ;define

(set! *load-path*
  (append (list "../common" "tools/common")
    (map (lambda (root) (string-append (%goldfmt-common-dirname root) "/common"))
      *load-path*
    ) ;map
    *load-path*
  ) ;append
) ;set!

(define-library (liii goldfmt)
  (import (liii base)
    (liii sys)
    (liii path)
    (liii string)
    (liii argparse)
    (liii hashlib)
    (liii goldtool-changed)
    (srfi srfi-13)
    (liii raw-string)
    (liii goldfmt-scan)
    (liii goldfmt-format)
  ) ;import
  (export main format-datum format-datum+node format-node format-string)
  (begin

    ;; ; 缓存工具函数
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

    ;; ; 显示帮助文档
    (define (display-help)
      (display "Usage: gf fmt [options] [path]")
      (newline)
      (newline)
      (display "Options:")
      (newline)
      (display "  -h, --help       显示此帮助文档")
      (newline)
      (display "      --dry-run    预览模式（不写回文件；目录路径不支持）"
      ) ;display
      (newline)
      (display "  -e, --extension EXT    指定文件后缀名（默认 scm，支持逗号分隔多个）"
      ) ;display
      (newline)
      (display "      --changed-since REV    仅格式化自 REV 以来变更的文件"
      ) ;display
      (newline)
      (display "      --exclude PATTERN    跳过匹配的文件（路径后缀匹配，逗号分隔多个）"
      ) ;display
      (newline)
      (newline)
      (display "Arguments:")
      (newline)
      (display "  path    要格式化的文件或目录路径（可选）")
      (newline)
      (newline)
      (display "Examples:")
      (newline)
      (display "  gf fmt                       显示此帮助文档")
      (newline)
      (display "  gf fmt --help                显示此帮助文档")
      (newline)
      (display "  gf fmt file.scm              格式化单个文件")
      (newline)
      (display "  gf fmt --dry-run file.scm    预览格式化结果")
      (newline)
      (display "  gf fmt --changed-since=HEAD  格式化自 HEAD 以来变更的文件"
      ) ;display
      (newline)
      (display "  gf fmt --dry-run --changed-since=HEAD  预览变更文件的格式化结果"
      ) ;display
      (newline)
      (display "  gf fmt /path/to/dir          递归格式化目录下所有 .scm 文件"
      ) ;display
      (newline)
      (display "  gf fmt -e sld /path/to/dir   递归格式化目录下所有 .sld 文件"
      ) ;display
      (newline)
      (display "  gf fmt -e scm,sld /path/to/dir  递归格式化目录下所有 .scm 和 .sld 文件"
      ) ;display
      (newline)
      (display "  gf fmt --exclude=prefix-kbd.scm /path/to/dir  递归格式化但跳过 prefix-kbd.scm"
      ) ;display
      (newline)
    ) ;define

    ;; ; 格式化单个文件（dry-run 模式，输出到终端）
    (define (format-file-dry-run path-str)
      (let* ((nodes (scan-file path-str)) (formatted (format-nodes nodes)))
        (display formatted)
      ) ;let*
    ) ;define

    ;; ; 格式化单个文件（覆盖原文件）
    ;; ; 返回值: 'cached (缓存命中), #t (文件有变更), #f (无变更)
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

    ;; ; 递归格式化指定文件列表
    ;; ; 返回值: (values total updated cached) - 处理的文件总数、更新数、缓存命中数
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
          (if (string-suffix? (car exts) filename) #t (loop (cdr exts)))
        ) ;if
      ) ;let
    ) ;define

    ;; 把 Windows 反斜杠归一化为正斜杠，跨平台一致比较。
    (define (normalize-sep path-str)
      (list->string
        (let loop
          ((i (- (string-length path-str) 1)) (acc '()))
          (if (< i 0)
            acc
            (let ((ch (string-ref path-str i)))
              (loop (- i 1) (cons (if (char=? ch #\\) #\/ ch) acc))
            ) ;let
          ) ;if
        ) ;let
      ) ;list->string
    ) ;define

    ;; entry 是否命中 excludes：归一化后按"分隔符边界 + 后缀"匹配。
    ;; pattern 含 / 时按完整相对路径精确匹配（不会误伤同名文件）；
    ;; pattern 不含 / 时退化为 basename 匹配（向后兼容）。
    (define (file-excluded? entry-str excludes)
      (let ((entry-norm (normalize-sep entry-str)))
        (let loop
          ((pats excludes))
          (if (null? pats)
            #f
            (let* ((p (normalize-sep (car pats)))
                   (plen (string-length p))
                   (elen (string-length entry-norm))
                  ) ;
              (if (or (string=? entry-norm p)
                      (and (>= elen (+ plen 1))
                           (char=? (string-ref entry-norm (- elen plen 1)) #\/)
                           (string-suffix? p entry-norm)))
                #t
                (loop (cdr pats))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 解析 --exclude 的逗号分隔值，丢弃空串。
    (define (parse-excludes raw)
      (if (or (not raw) (string=? raw ""))
        '()
        (let loop
          ((parts (string-split raw ",")) (acc '()))
          (if (null? parts)
            (reverse acc)
            (let ((p (car parts)))
              (if (string=? p "")
                (loop (cdr parts) acc)
                (loop (cdr parts) (cons p acc))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (format-changed-since since path-str dry-run extensions excludes)
      (let ((scope (if (string=? path-str "") #f path-str)))
        (cond ((and scope (not (or (path-file? (path scope)) (path-dir? (path scope)))))
               (display (string-append "错误: 路径不存在 - " scope))
               (newline)
               (exit 1)
              ) ;
              (else (let ((files (if scope
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
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; ; 递归格式化目录
    ;; ; 返回值: (values total updated cached) - 处理的文件总数、更新数、缓存命中数
    (define (format-directory dir-path extensions excludes)
      (let ((entries (path-list-path (path dir-path))))
        (let loop
          ((i 0) (total 0) (updated 0) (cached 0))
          (if (>= i (vector-length entries))
            (values total updated cached)
            (let ((entry (vector-ref entries i)))
              (cond ((path-file? entry)
                     (let ((entry-str (path->string entry)))
                       (if (and (file-extension-match? entry-str extensions)
                                (not (file-excluded? entry-str excludes)))
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
                     (call-with-values (lambda () (format-directory (path->string entry) extensions excludes))
                       (lambda (sub-total sub-updated sub-cached)
                         (loop (+ i 1) (+ total sub-total) (+ updated sub-updated) (+ cached sub-cached))
                       ) ;lambda
                     ) ;call-with-values
                    ) ;
                    (else (loop (+ i 1) total updated cached))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (make-fmt-arg-parser)
      (let ((parser (make-argument-parser '((command . "fmt")
                                            (skip-value-options "-m"
                                              "--mode"
                                              "-I"
                                              "-A")
                                            (skip-prefix-options "-m="
                                              "--mode=")
                                            (unknown-options . positional))
                    ) ;make-argument-parser
            ) ;parser
           ) ;
        (parser :add-argument '((name . "help")
                                (short . "h")
                                (action . store-true)))
        (parser :add-argument '((name . "dry-run") (action . store-true)))
        (parser :add-argument
          '((name . "extension")
            (short . "e")
            (type . string)
            (default . "scm"))
        ) ;parser
        (parser :add-argument '((name . "changed-since") (type . string)))
        (parser :add-argument '((name . "exclude") (type . string)))
        parser
      ) ;let
    ) ;define

    (define (first-positional parser)
      (let ((positionals (parser :positionals)))
        (if (null? positionals) "" (car positionals))
      ) ;let
    ) ;define

    (define (normalize-extension ext)
      (if (and (> (string-length ext) 0) (char=? (string-ref ext 0) #\.))
        ext
        (string-append "." ext)
      ) ;if
    ) ;define

    (define (parse-extensions raw)
      (map normalize-extension (string-split raw ","))
    ) ;define

    ;; ; 主入口函数
    (define (main)
      (let ((parser (make-fmt-arg-parser)))
        (parser :parse-argv (argv))
        (let* ((help-flag (parser 'help))
               (dry-run (parser 'dry-run))
               (extensions (parse-extensions (parser 'extension)))
               (changed-since (parser 'changed-since))
               (excludes (parse-excludes (parser 'exclude)))
               (path-str (first-positional parser))
              ) ;
          (cond (help-flag (display-help) #t)
                (changed-since (format-changed-since changed-since path-str dry-run extensions excludes))
                ((string=? path-str "") (display-help) #t)
                ((path-file? (path path-str))
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
                ) ;
                ((path-dir? (path path-str))
                 (if dry-run
                   (begin
                     (display "错误: --dry-run 选项仅支持单个文件")
                     (newline)
                     (exit 1)
                   ) ;begin
                   (call-with-values (lambda () (format-directory path-str extensions excludes))
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
                ) ;
                (else (display (string-append "错误: 路径不存在 - " path-str))
                  (newline)
                  (exit 1)
                ) ;else
          ) ;cond
        ) ;let*
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
