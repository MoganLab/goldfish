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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(import (liii base)
  (liii os)
  (liii path)
  (liii timeit)
  (only (srfi srfi-13) string-suffix? string-contains)
) ;import

;; 递归收集目录下所有 .scm 文件

(define (collect-scm-files dir)
  (let loop
    ((acc '()) (entries (vector->list (listdir dir))))
    (if (null? entries)
      acc
      (let ((p (string-append dir (string (os-sep)) (car entries))))
        (cond ((and (path-dir? p)
                 (not (string-contains p (string-append (string (os-sep)) "resources")))
               ) ;and
               ;; 跳过 resources 目录：其中包含故意损坏的括号测试文件
               (loop (append (collect-scm-files p) acc) (cdr entries))
              ) ;
              ((string-suffix? ".scm" (car entries)) (loop (cons p acc) (cdr entries)))
              (else (loop acc (cdr entries)))
        ) ;cond
      ) ;let
    ) ;if
  ) ;let
) ;define

(define (report title iterations time-val)
  (display "[")
  (display title)
  (display "] iterations=")
  (display iterations)
  (display " time=")
  (display time-val)
  (display "s")
  (newline)
) ;define

(define (read-all-from-file file)
  ;; 个别测试文件故意包含语法错误（括号不匹配、未闭合字符串等），读取出错时跳过
  (catch #t
    (lambda ()
      (let ((p (open-input-file file)))
        (let loop
          ()
          (let ((x (read p)))
            (if (eof-object? x) (begin (close-input-port p) 'done) (loop))
          ) ;let
        ) ;let
      ) ;let
    ) ;lambda
    (lambda args 'skipped)
  ) ;catch
) ;define

(define bench-dirs '("goldfish" "tests"))

(define scm-files (apply append (map collect-scm-files bench-dirs)))

(display "=== read 性能基准测试 ===")
(newline)
(display "文件: ")
(display (length scm-files))
(display " 个 .scm 文件 (goldfish/ + tests/)")
(newline)
(newline)

(define (read-all-files)
  (for-each read-all-from-file scm-files)
) ;define

;; warmup
(read-all-files)

(define read-iter 10)

(let ((t (timeit (lambda () (read-all-files)) '() read-iter)))
  (report "读取全部文件(open-input-file+read)" read-iter t)
) ;let

(newline)
(display "=== 测试完成 ===")
(newline)
