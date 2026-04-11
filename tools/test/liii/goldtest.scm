
;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii goldtest)
  (import (scheme base)
          (scheme process-context)
          (liii sort)
          (liii list)
          (liii string)
          (liii os)
          (liii path)
          (liii sys)
  ) ;import
  (export parse-test-args
          filter-test-files
          find-test-files
          run-goldtest
          main
  ) ;export
  (begin

    (define ESC (string #\escape #\[))
    
    (define (color code)
      (string-append ESC (number->string code) "m")
    ) ;define
    
    (define GREEN (color 32))
    (define RED (color 31))
    (define YELLOW (color 33))
    (define RESET (color 0))
    
    (define (test-path-join . parts)
      (let ((sep (string (os-sep))))
        (let loop ((result "")
                   (rest parts))
          (if (null? rest)
            result
            (let ((part (car rest)))
              (if (string-null? result)
                (loop part (cdr rest))
                (loop (string-append result sep part) (cdr rest))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    
    (define (find-test-files dir)
      (let ((files '()))
        (when (path-dir? dir)
          (let ((entries (listdir dir)))
            (for-each
              (lambda (entry)
                (let ((full-path (test-path-join dir entry)))
                  (cond
                    ((path-dir? full-path)
                     (set! files (append files (find-test-files full-path)))
                    ) ;
                    ((and (path-file? full-path)
                          (string-ends? entry "-test.scm"))
                     (set! files (cons full-path files))
                    ) ;
                  ) ;cond
                ) ;let
              ) ;lambda
              entries
            ) ;for-each
          ) ;let
        ) ;when
        files
      ) ;let
    ) ;define
    
    (define (goldfish-cmd)
      (string-append (executable) " -m r7rs ")
    ) ;define
    
    (define (run-test-file test-file)
      (let ((cmd (string-append (goldfish-cmd) test-file)))
        (display "----------->") (newline)
        (display cmd) (newline)
        (let ((result (os-call cmd)))
          (cons test-file result)
        ) ;let
      ) ;let
    ) ;define
    
    (define (display-summary test-results)
      (let ((total (length test-results))
            (passed (count (lambda (x) (zero? (cdr x))) test-results))
            (failed (- (length test-results)
                       (count (lambda (x) (zero? (cdr x))) test-results)))
            ) ;failed
        (newline)
        (display "=== Test Summary ===") (newline)
        (newline)
        (for-each
          (lambda (test-result)
            (let ((test-file (car test-result))
                  (exit-code (cdr test-result)))
              (display (string-append "  " test-file " ... "))
              (if (zero? exit-code)
                (display (string-append GREEN "PASS" RESET))
                (display (string-append RED "FAIL" RESET))
              ) ;if
              (newline)
            ) ;let
          ) ;lambda
          test-results
        ) ;for-each
        (newline)
        (display "=== Summary ===") (newline)
        (display (string-append "  Total:  " (number->string total))) (newline)
        (display (string-append "  " GREEN "Passed: " (number->string passed) RESET)) (newline)
        (when (> failed 0)
          (display (string-append "  " RED "Failed: " (number->string failed) RESET)) (newline)
        ) ;when
        (newline)
        failed
      ) ;let
    ) ;define
    
    (define (parse-test-args args)
      ;; 解析 test 命令的参数
      ;; 规则：
      ;; 1. 如果参数包含 /，视为路径处理
      ;;    - 如果是存在的文件，直接返回该文件
      ;;    - 如果是存在的目录，返回该目录用于后续查找
      ;; 2. 如果参数以 .scm 结尾但不是路径，按文件名匹配
      ;; 3. 其他情况，按模糊匹配（路径中包含该字符串）
      ;; 返回值: (type . value)
      ;;   type 可以是: 'file, 'dir, 'filename, 'pattern, #f
      ;; args 的第一个元素是可执行文件路径，需要跳过
      (if (null? args)
        (cons #f #f)
        (let loop ((remaining (cdr args)) ; 跳过第一个参数（可执行文件）
                   (skip-next #f))        ; 是否跳过下一个参数（模式值）
          (if (null? remaining)
            (cons #f #f)
            (let ((arg (car remaining)))
              (cond
                ;; 如果需要跳过当前参数（作为 -m/--mode 的值）
                (skip-next
                 (loop (cdr remaining) #f)
                ) ;skip-next
                ;; 跳过 test 命令
                ((equal? arg "test")
                 (loop (cdr remaining) #f)
                ) ;
                ;; -m 或 --mode 后面需要跳过模式值
                ((or (equal? arg "-m") (equal? arg "--mode"))
                 (loop (cdr remaining) #t)
                ) ;
                ;; -m=... 或 --mode=... 格式，跳过当前参数
                ((or (string-starts? arg "-m=") (string-starts? arg "--mode="))
                 (loop (cdr remaining) #f)
                ) ;
                ;; 包含路径分隔符的路径 (/ 或 Windows 的 \)
                ((or (string-contains arg "/")
                     (and (os-windows?) (string-contains arg "\\")))
                 (let ((abs-path (if (path-absolute? arg)
                                     arg
                                     (path-join (getcwd) arg))))
                   (cond
                     ((path-file? abs-path) (cons 'file abs-path))
                     ((path-dir? abs-path) (cons 'dir abs-path))
                     (else (cons 'pattern arg)) ; 不存在的路径，按模式匹配
                   ) ;cond
                 ) ;let
                ) ;
                ;; 以 .scm 结尾的文件名
                ((string-ends? arg ".scm")
                 (cons 'filename arg)
                ) ;
                ;; 其他视为模糊匹配模式
                (else
                 (cons 'pattern arg)
                ) ;else
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define
    
    (define (filter-test-files test-files arg-type arg-value)
      ;; 根据参数类型过滤测试文件
      (case arg-type
        ((file)
         ;; 直接返回单个文件（已经在 parse-test-args 中验证存在）
         (list arg-value)
        ) ;
        ((dir)
         ;; 返回该目录下的所有测试文件
         ;; 在 Windows 上，将用户输入的正斜杠转换为反斜杠以匹配文件路径
         (let ((dir-pattern (if (os-windows?)
                                (string-replace arg-value "/" "\\")
                                arg-value)))
           (filter (lambda (file) (string-starts? file dir-pattern)) test-files)
         ) ;let
        ) ;
        ((filename)
         ;; 精确匹配文件名
         (filter (lambda (file) (string=? (path-name file) arg-value)) test-files)
        ) ;
        ((pattern)
         ;; 模糊匹配路径
         (filter (lambda (file) (string-contains file arg-value)) test-files)
        ) ;
        (else
         ;; 无参数，返回所有文件
         test-files
        ) ;else
      ) ;case
    ) ;define
    
    (define (display-filter-info arg-type arg-value)
      ;; 显示过滤信息
      (case arg-type
        ((file)
         (display (string-append "Running test file: " arg-value))
         (newline)
        ) ;
        ((dir)
         (display (string-append "Running tests in directory: " arg-value))
         (newline)
        ) ;
        ((filename)
         (display (string-append "Running tests with file name: " arg-value))
         (newline)
        ) ;
        ((pattern)
         (display (string-append "Running tests matching pattern: " arg-value))
         (newline)
        ) ;
      ) ;case
    ) ;define
    
    (define (run-goldtest)
      (let* ((args (command-line))
             (parsed (parse-test-args args))
             (arg-type (car parsed))
             (arg-value (cdr parsed))
             (all-test-files (list-sort string<? (find-test-files "tests")))
             (test-files (filter-test-files all-test-files arg-type arg-value)))
        (if (null? test-files)
          (begin
            (if arg-value
              (begin
                (display (string-append YELLOW "No test files matching " arg-value RESET))
                (newline)
              ) ;begin
              (begin
                (display (string-append YELLOW "No test files found in tests directory" RESET))
                (newline)
              ) ;begin
            ) ;if
            (exit 0)
          ) ;begin
          (begin
            (when arg-value
              (display-filter-info arg-type arg-value)
            ) ;when
            (let ((test-results
                    (fold (lambda (test-file acc)
                            (newline)
                            (cons (run-test-file test-file) acc))
                          (list)
                          test-files))
                    ) ;fold
              (let ((failed (display-summary test-results)))
                (exit (if (> failed 0) -1 0))
              ) ;let
            ) ;let
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define
    
    (define (show-help)
      ;; 显示帮助信息
      (display "gf test - Goldfish Scheme Test Runner")
      (newline) (newline)
      (display "Usage:") (newline)
      (display "  gf test [PATH|PATTERN]") (newline) (newline)
      (display "Examples:") (newline)
      (display "  gf test                    Run all tests") (newline)
      (display "  gf test tests/liii/string  Run tests in directory") (newline)
      (display "  gf test string-test.scm    Run specific test file") (newline)
      (display "  gf test string             Run tests matching pattern") (newline)
    ) ;define

    (define (main)
      ;; 程序入口点
      (let ((args (command-line)))
        (if (or (member "--help" args) (member "-h" args))
            (begin (show-help) (exit 0))
            (run-goldtest)
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
