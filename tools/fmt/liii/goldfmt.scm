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
    (liii goldtool-changed)
    (srfi srfi-13)
    (liii raw-string)
    (liii goldfmt-scan)
    (liii goldfmt-format)
  ) ;import
  (export main format-datum format-datum+node format-node format-string)
  (begin

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
      (display "      --changed-since REV    仅格式化自 REV 以来变更的 .scm 文件"
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
      (display "  gf fmt --changed-since=HEAD  格式化自 HEAD 以来变更的 .scm 文件"
      ) ;display
      (newline)
      (display "  gf fmt --dry-run --changed-since=HEAD  预览变更文件的格式化结果"
      ) ;display
      (newline)
      (display "  gf fmt /path/to/dir          递归格式化目录下所有 .scm 文件"
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
    ;; ; 返回值: 如果文件有变更返回 #t，否则返回 #f
    (define (format-file path-str)
      (let* ((p (path path-str))
             (original-content (path-read-text p))
             (nodes (scan-file path-str))
             (formatted (format-nodes nodes))
            ) ;
        (if (string=? original-content formatted)
          #f
          (begin
            (path-write-text p formatted)
            #t
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    ;; ; 递归格式化指定文件列表
    ;; ; 返回值: (values total updated) - 处理的文件总数和更新的文件数
    (define (format-file-list files dry-run)
      (let loop
        ((remaining files) (total 0) (updated 0))
        (if (null? remaining)
          (values total updated)
          (let ((file (car remaining)))
            (display (string-append "Formatting: " file))
            (newline)
            (if dry-run
              (begin
                (format-file-dry-run file)
                (loop (cdr remaining) (+ total 1) updated)
              ) ;begin
              (let ((changed? (format-file file)))
                (if changed?
                  (begin
                    (display (string-append "  Updated: " file))
                    (newline)
                    (loop (cdr remaining) (+ total 1) (+ updated 1))
                  ) ;begin
                  (loop (cdr remaining) (+ total 1) updated)
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (format-changed-since since path-str dry-run)
      (let ((scope (if (string=? path-str "") #f path-str)))
        (cond ((and scope (not (or (path-file? (path scope)) (path-dir? (path scope)))))
               (display (string-append "错误: 路径不存在 - " scope))
               (newline)
               (exit 1)
              ) ;
              (else (let ((files (if scope
                                   (changed-scheme-files-since since scope)
                                   (changed-scheme-files-since since)
                                 ) ;if
                          ) ;files
                         ) ;
                      (if (null? files)
                        (begin
                          (display (string-append "No changed Scheme files since " since))
                          (newline)
                          #t
                        ) ;begin
                        (call-with-values (lambda () (format-file-list files dry-run))
                          (lambda (total updated)
                            (display (string-append "Total files formatted: "
                                       (number->string total)
                                       ", Files updated: "
                                       (number->string updated)
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
    ;; ; 返回值: (values total updated) - 处理的文件总数和更新的文件数
    (define (format-directory dir-path)
      (let ((entries (path-list-path (path dir-path))))
        (let loop
          ((i 0) (total 0) (updated 0))
          (if (>= i (vector-length entries))
            (values total updated)
            (let ((entry (vector-ref entries i)))
              (cond ((path-file? entry)
                     (let ((entry-str (path->string entry)))
                       (if (string-suffix? ".scm" entry-str)
                         (begin
                           (display (string-append "Formatting: " entry-str))
                           (newline)
                           (let ((changed? (format-file entry-str)))
                             (if changed?
                               (begin
                                 (display (string-append "  Updated: " entry-str))
                                 (newline)
                                 (loop (+ i 1) (+ total 1) (+ updated 1))
                               ) ;begin
                               (loop (+ i 1) (+ total 1) updated)
                             ) ;if
                           ) ;let
                         ) ;begin
                         (loop (+ i 1) total updated)
                       ) ;if
                     ) ;let
                    ) ;
                    ((path-dir? entry)
                     (call-with-values (lambda () (format-directory (path->string entry)))
                       (lambda (sub-total sub-updated)
                         (loop (+ i 1) (+ total sub-total) (+ updated sub-updated))
                       ) ;lambda
                     ) ;call-with-values
                    ) ;
                    (else (loop (+ i 1) total updated))
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
        (parser :add-argument '((name . "changed-since") (type . string)))
        parser
      ) ;let
    ) ;define

    (define (first-positional parser)
      (let ((positionals (parser :positionals)))
        (if (null? positionals) "" (car positionals))
      ) ;let
    ) ;define

    ;; ; 主入口函数
    (define (main)
      (let ((parser (make-fmt-arg-parser)))
        (parser :parse-argv (argv))
        (let ((help-flag (parser 'help))
              (dry-run (parser 'dry-run))
              (changed-since (parser 'changed-since))
              (path-str (first-positional parser))
             ) ;
          (cond (help-flag (display-help) #t)
                (changed-since (format-changed-since changed-since path-str dry-run))
                ((string=? path-str "") (display-help) #t)
                ((path-file? (path path-str))
                 (if dry-run
                   (format-file-dry-run path-str)
                   (begin
                     (display (string-append "Formatting: " path-str))
                     (newline)
                     (let ((changed? (format-file path-str)))
                       (if changed?
                         (begin
                           (display (string-append "  Updated: " path-str))
                           (newline)
                         ) ;begin
                         '()
                       ) ;if
                       (display (string-append "Total files formatted: 1, Files updated: "
                                  (if changed? "1" "0")
                                ) ;string-append
                       ) ;display
                       (newline)
                       #t
                     ) ;let
                   ) ;begin
                 ) ;if
                ) ;
                ((path-dir? (path path-str))
                 (if dry-run
                   (begin
                     (display "错误: --dry-run 选项仅支持单个文件")
                     (newline)
                     (exit 1)
                   ) ;begin
                   (call-with-values (lambda () (format-directory path-str))
                     (lambda (total updated)
                       (display (string-append "Total files formatted: "
                                  (number->string total)
                                  ", Files updated: "
                                  (number->string updated)
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
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
