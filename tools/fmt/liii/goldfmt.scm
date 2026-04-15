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

(define-library (liii goldfmt)
  (import (liii base)
          (liii sys)
          (liii path)
          (liii string)
          (srfi srfi-13)
          (liii goldfmt-scan)
          (liii goldfmt-format))
  (export main
          format-datum
          format-datum+node
          format-node
          format-string)
  (begin
    
    ;;; 显示帮助文档
    (define (display-help)
      (display "Usage: gf fmt [options] [path]")
      (newline)
      (newline)
      (display "Options:")
      (newline)
      (display "  -h, --help       显示此帮助文档")
      (newline)
      (display "      --dry-run    预览模式（仅支持单个文件）")
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
      (display "  gf fmt /path/to/dir          递归格式化目录下所有 .scm 文件")
      (newline))
    
    ;;; 格式化单个文件（dry-run 模式，输出到终端）
    (define (format-file-dry-run path-str)
      (let* ((nodes (scan-file path-str)))
        (let loop ((i 0))
          (if (>= i (vector-length nodes))
              (values)
              (let ((node (vector-ref nodes i)))
                (call-with-values
                  (lambda () (format-node node 0))
                  (lambda (text positioned-node)
                    (display text)
                    (newline)
                    (loop (+ i 1)))))))))
    
    ;;; 格式化单个文件（覆盖原文件）
    ;;; 返回值: 如果文件有变更返回 #t，否则返回 #f
    (define (format-file path-str)
      (let* ((p (path path-str))
             (original-content (path-read-text p))
             (nodes (scan-file path-str))
             (results '()))
        (let loop ((i 0)
                   (acc '()))
          (if (>= i (vector-length nodes))
              (let ((formatted (string-join (reverse acc) "\n")))
                (if (string=? original-content formatted)
                    #f  ; 无变更
                    (begin
                      (path-write-text p formatted)
                      #t)))  ; 有变更
              (let ((node (vector-ref nodes i)))
                (call-with-values
                  (lambda () (format-node node 0))
                  (lambda (text positioned-node)
                    (loop (+ i 1) (cons text acc))))))))) 
    
    ;;; 递归格式化目录
    (define (format-directory dir-path)
      (let ((entries (path-list-path (path dir-path))))
        (vector-for-each
          (lambda (entry)
            (cond
              ((path-file? entry)
               (let ((entry-str (path->string entry)))
                 (if (string-suffix? ".scm" entry-str)
                     (begin
                       (display (string-append "Processing: " entry-str))
                       (newline)
                       (let ((changed? (format-file entry-str)))
                         (if changed?
                             (display (string-append "Updated: " entry-str))
                             (display (string-append "Unchanged: " entry-str)))
                         (newline))))))
              ((path-dir? entry)
               (format-directory (path->string entry)))))
          entries)))
    
    ;;; 检查列表中是否包含某个元素
    (define (contains? lst item)
      (if (null? lst)
          #f
          (or (string=? (car lst) item)
              (contains? (cdr lst) item))))
    
    ;;; 从参数列表中提取非标志参数（位置参数）
    (define (extract-positional args)
      (cond
        ((null? args) "")
        ((or (string=? (car args) "-h") 
             (string=? (car args) "--help")
             (string=? (car args) "--dry-run"))
         (extract-positional (cdr args)))
        (else (car args))))
    
    ;;; 主入口函数
    (define (main)
      (let* ((args (cddr (argv)))  ; 跳过 "gf" 和 "fmt"
             (help-flag (or (contains? args "-h") (contains? args "--help")))
             (dry-run (contains? args "--dry-run"))
             (path-str (extract-positional args)))
        (cond
          ; 显示帮助
          ((or help-flag (string=? path-str ""))
           (display-help)
           #t)
          ; 处理单个文件
          ((path-file? (path path-str))
           (if dry-run
               (format-file-dry-run path-str)
               (begin
                 (display (string-append "Processing: " path-str))
                 (newline)
                 (let ((changed? (format-file path-str)))
                   (if changed?
                       (display (string-append "Updated: " path-str))
                       (display (string-append "Unchanged: " path-str)))
                   (newline)
                   #t))))
          ; 处理目录（不支持 --dry-run）
          ((path-dir? (path path-str))
           (if dry-run
               (begin
                 (display "错误: --dry-run 选项仅支持单个文件")
                 (newline)
                 (exit 1))
               (begin
                 (format-directory path-str)
                 #t)))
          ; 路径不存在
          (else
           (display (string-append "错误: 路径不存在 - " path-str))
           (newline)
           (exit 1)))))
  ) ;begin
) ;define-library
