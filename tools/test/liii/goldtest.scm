
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

(define (%goldtest-common-dirname path-str)
  (let loop ((i (- (string-length path-str) 1)))
    (cond ((< i 0) ".")
          ((or (char=? (string-ref path-str i) #\/)
               (char=? (string-ref path-str i) #\\))
           (if (= i 0) "." (substring path-str 0 i))
          ) ;
          (else (loop (- i 1)))
    ) ;cond
  ) ;let
) ;define

(set! *load-path*
  (append (list "../common" "tools/common")
    (map (lambda (root)
           (string-append (%goldtest-common-dirname root) "/common")
         ) ;lambda
      *load-path*
    ) ;map
    *load-path*
  ) ;append
) ;set!

(define-library (liii goldtest)
  (import (scheme base)
          (scheme process-context)
          (liii sort)
          (liii list)
          (liii string)
          (liii argparse)
          (liii goldtool-changed)
          (liii os)
          (liii path)
          (liii sys)
  ) ;import
  (export parse-test-args
          parse-test-changed-since
          filter-test-files
          filter-changed-test-files
          find-test-files
          failed-test-files
          split-tests-target
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

    (define (failed-test-files test-results)
      (map car
           (filter (lambda (test-result)
                     (not (zero? (cdr test-result))))
                   test-results))
    ) ;define
    
    (define (display-summary test-results)
      (let ((total (length test-results))
            (passed (count (lambda (x) (zero? (cdr x))) test-results))
            (failed-files (failed-test-files test-results))
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
          (display "  Failed Test Files:") (newline)
          (for-each
            (lambda (test-file)
              (display (string-append "    " test-file)) (newline)
            ) ;lambda
            failed-files
          ) ;for-each
        ) ;when
        (newline)
        failed
      ) ;let
    ) ;define
    
    (define (make-test-arg-parser)
      (let ((parser (make-argument-parser
                      '((command . "test")
                        (skip-value-options . ("-m" "--mode"))
                        (skip-prefix-options . ("-m=" "--mode="))
                        (unknown-options . positional)))))
        (parser :add-argument
          '((name . "changed-since") (type . string))
        ) ;parser
        (parser :add-argument
          '((name . "all") (action . store-true))
        ) ;parser
        (parser :add-argument
          '((name . "help") (short . "h") (action . store-true))
        ) ;parser
        parser
      ) ;let
    ) ;define

    (define (classify-test-arg arg)
      (cond
        ;; 包含路径分隔符的路径 (/ 或 Windows 的 \)
        ((or (string-contains arg "/")
             (and (os-windows?) (string-contains arg "\\")))
         (let ((abs-path (if (path-absolute? arg)
                           arg
                           (path-join (getcwd) arg))))
           (cond
             ((path-file? abs-path) (cons 'file arg))  ; 保留原始路径（可能是相对路径）
             ((path-dir? abs-path) (cons 'dir arg))    ; 保留原始路径（可能是相对路径）
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
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (let ((positionals (parser :positionals)))
          (if (null? positionals)
            (cons #f #f)
            (classify-test-arg (car positionals))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (parse-test-changed-since args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (parser 'changed-since)
      ) ;let
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

    (define (normalize-test-file-path file)
      (if (os-windows?)
        (string-replace file "\\" "/")
        file
      ) ;if
    ) ;define

    (define (filter-changed-test-files test-files since)
      (let ((changed-files (changed-scheme-files-since since)))
        (filter (lambda (file)
                  (member (normalize-test-file-path file) changed-files)
                ) ;lambda
          test-files
        ) ;filter
      ) ;let
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

    (define (split-tests-target target)
      ;; 将 .../tests 或 .../tests/... 路径拆成父目录和相对 tests 路径
      (let ((marker-length 6)) ; "/tests" or "\tests"
        (let loop ((i 0))
          (cond
            ((> i (- (string-length target) marker-length))
             #f
            ) ;
            ((and (or (string=? (substring target i (+ i marker-length)) "/tests")
                      (string=? (substring target i (+ i marker-length)) "\\tests"))
                  (or (= (+ i marker-length) (string-length target))
                      (char=? (string-ref target (+ i marker-length)) #\/)
                      (char=? (string-ref target (+ i marker-length)) #\\)))
             (let ((parent (substring target 0 i))
                   (next-pos (+ i marker-length)))
               (if (> (string-length parent) 0)
                 (let ((tests-path (substring target (+ i 1))))
                   (if (= next-pos (string-length target))
                     (cons parent
                           (string-append tests-path (string (string-ref target i))))
                     (cons parent tests-path)
                   ) ;if
                 ) ;let
                 #f
               ) ;if
             ) ;let
            ) ;
            (else
             (loop (+ i 1))
            ) ;else
          ) ;cond
        ) ;let loop
      ) ;let
    ) ;define
    
    (define (check-and-switch-to-target args)
      ;; 检查是否需要切换到 target 目录
      ;; 规则：
      ;; 1. 如果第一个非选项参数是目录，且该目录下有 tests 子目录，则切换
      ;; 2. 如果参数路径以 /tests 或 /tests/ 结尾，提取父目录作为 target 并切换
      ;; 返回切换后的新参数列表（如果切换了，需要去掉或修改 target 参数）
      (let loop ((remaining (cdr args))  ; 跳过可执行文件路径
                 (skip-next #f)
                 (found-target #f))
        (cond
          ;; 没有更多参数
          ((null? remaining)
           (if found-target
             (let* ((target found-target)
                    (tests-target (split-tests-target target)))
               (cond
                 ;; 情况 1: 路径包含 /tests/，按原有逻辑处理
                 (tests-target
                  (let ((parent (car tests-target))
                        (tests-path (cdr tests-target)))
                    (if (path-dir? parent)
                      (begin
                        (chdir parent)
                        ;; 切换目录后，将参数改为 tests/...（相对路径）
                        (cons (car args)
                              (map (lambda (arg)
                                     (if (equal? arg target) tests-path arg))
                                   (cdr args)))
                      ) ;begin
                      args
                    ) ;if
                  ) ;let
                 ) ;tests-target
                 ;; 情况 2: 目标是目录且包含 tests 子目录
                 ((and (path-dir? target)
                       (path-dir? (test-path-join target "tests")))
                  (chdir target)
                  ;; 切换目录后，将参数中的 target 替换为 "tests"
                  (cons (car args)
                        (map (lambda (arg)
                               (if (equal? arg target) "tests" arg))
                             (cdr args)))
                 ) ;dir with tests
                 ;; 其他情况，不切换
                 (else args)
               ) ;cond
             ) ;let
             args
           ) ;if
          ) ;null?
          ;; 跳过选项值
          (skip-next
           (loop (cdr remaining) #f found-target)
          ) ;skip-next
          ;; 跳过 test 命令本身
          ((equal? (car remaining) "test")
           (loop (cdr remaining) #f found-target)
          ) ;test
          ;; 跳过 -m/--mode 及其值
          ((or (equal? (car remaining) "-m") (equal? (car remaining) "--mode"))
           (loop (if (null? (cdr remaining)) '() (cddr remaining)) #f found-target)
          ) ;-m/--mode
          ;; 跳过 -m=.../--mode=... 格式
          ((or (string-starts? (car remaining) "-m=")
               (string-starts? (car remaining) "--mode="))
           (loop (cdr remaining) #f found-target)
          ) ;-m=...
          ;; 跳过 --changed-since 及其值
          ((equal? (car remaining) "--changed-since")
           (loop (if (null? (cdr remaining)) '() (cddr remaining)) #f found-target)
          ) ;--changed-since
          ;; 跳过 --changed-since=... 格式
          ((string-starts? (car remaining) "--changed-since=")
           (loop (cdr remaining) #f found-target)
          ) ;--changed-since=...
          ;; 跳过 --all
          ((equal? (car remaining) "--all")
           (loop (cdr remaining) #f found-target)
          ) ;--all
          ;; 找到 target（第一个非选项参数）
          ((not found-target)
           (loop (cdr remaining) #f (car remaining))
          ) ;not found-target
          ;; 其他参数，继续
          (else
           (loop (cdr remaining) #f found-target)
          ) ;else
        ) ;cond
      ) ;let loop
    ) ;define

    (define (git-current-branch)
      (let* ((tmp-file (test-path-join (os-temp-dir)
                                       (string-append "gf-test-branch-"
                                                      (number->string (getpid))
                                                      ".txt")))
             (cmd (string-append "git rev-parse --abbrev-ref HEAD > "
                                 tmp-file
                                 " 2>&1"))
             (exit-code (os-call cmd)))
        (if (zero? exit-code)
            (let* ((port (open-input-file tmp-file))
                   (branch (read-line port)))
              (close-input-port port)
              (remove tmp-file)
              branch
            ) ;let*
            (begin
              (when (file-exists? tmp-file)
                (remove tmp-file)
              ) ;when
              #f
            ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (parse-test-all args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (parser 'all)
      ) ;let
    ) ;define

    (define (route-test-command args all-mode)
      (let ((branch (git-current-branch))
            (exe (executable)))
        (cond
          ((and branch (not (string=? branch "main")))
           (if all-mode
               (begin
                 (display (string-append "[gf test] Not on main branch (currently on '"
                                         branch
                                         "'), running all tests (--all). Use `gf test --help` for details."))
                 (newline)
                 (display (string-append "Running: " exe " test --changed-since=main"))
                 (newline)
                 (display (string-append "Running: " exe " test tests"))
                 (newline)
                 (newline)
                 (cons "main" #t)
               ) ;begin
               (begin
                 (display (string-append "[gf test] Not on main branch (currently on '"
                                         branch
                                         "'), running changed tests since main. Use `gf test --help` for details."))
                 (newline)
                 (display (string-append "Running: " exe " test --changed-since=main"))
                 (newline)
                 (newline)
                 (cons "main" #f)
               ) ;begin
           ) ;if
          ) ;non-main branch
          (branch
           (display "[gf test] On main branch, running all tests. Use `gf test --help` for details.")
           (newline)
           (display (string-append "Running: " exe " test tests"))
           (newline)
           (newline)
           (cons #f #f)
          ) ;main branch
          (else
           (display "[gf test] Not a git repository, running all tests. Use `gf test --help` for details.")
           (newline)
           (display (string-append "Running: " exe " test tests"))
           (newline)
           (newline)
           (cons #f #f)
          ) ;not git repo
        ) ;cond
      ) ;let
    ) ;define

    (define (run-goldtest)
      (let* ((raw-args (command-line))
             (args (check-and-switch-to-target raw-args))
             (all-mode (parse-test-all args))
             (changed-since (parse-test-changed-since args))
             (parsed (parse-test-args args))
             (arg-type (car parsed))
             (arg-value (cdr parsed))
             ;; 智能路由：无显式参数时根据 git 状态决定
             (route-result (if (and (not arg-type) (not changed-since))
                               (route-test-command args all-mode)
                               #f))
             (final-changed-since (if route-result (car route-result) changed-since))
             (need-run-all (if route-result (cdr route-result) #f))
             (all-test-files (list-sort string<? (find-test-files "tests")))
             (filtered-test-files (filter-test-files all-test-files arg-type arg-value))
             ;; 如果指定了 changed-since，先过滤出变更的测试
             (changed-test-files (if final-changed-since
                                     (filter-changed-test-files filtered-test-files final-changed-since)
                                     '()))
             ;; 在 --all 模式下，把未变更的测试追加在后面
             (remaining-test-files (if need-run-all
                                       (filter (lambda (f)
                                                 (not (member f changed-test-files)))
                                               filtered-test-files)
                                       '()))
             (test-files (if need-run-all
                             (append changed-test-files remaining-test-files)
                             (if final-changed-since
                                 changed-test-files
                                 filtered-test-files)))
            ) ;let*
        (if (null? test-files)
          (begin
            (if final-changed-since
              (begin
                (display (string-append YELLOW "No test files changed since " final-changed-since RESET))
                (newline)
              ) ;begin
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
            ) ;if
            (exit 0)
          ) ;begin
          (begin
            (when arg-value
              (display-filter-info arg-type arg-value)
            ) ;when
            (when final-changed-since
              (display (string-append "Running changed tests since: " final-changed-since))
              (newline)
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
      (display "  gf test [options] [PATH|PATTERN]") (newline) (newline)
      (display "Options:") (newline)
      (display "  --all                            Run all tests (greedy: changed first, then all)") (newline)
      (display "  --changed-since REV              Run tests changed since REV") (newline) (newline)
      (display "Examples:") (newline)
      (display "  gf test                          Run tests (smart route based on git branch)") (newline)
      (display "  gf test --all                    Run all tests") (newline)
      (display "  gf test tools/doc/tests/         Run tests in directory") (newline)
      (display "  gf test tests/liii/string/       Run tests in directory") (newline)
      (display "  gf test string-test.scm          Run specific test file") (newline)
      (display "  gf test string                   Run tests matching pattern") (newline)
      (display "  gf test --changed-since=HEAD     Run changed test files") (newline) (newline)
      (display "Note:") (newline)
      (display "  Smart routing: on non-main branch, gf test runs --changed-since=main") (newline)
      (display "  If path contains /tests/, it will switch to parent directory") (newline)
      (display "  e.g., gf test tools/doc/tests/  =>  cd tools/doc && gf test tests/") (newline)
    ) ;define

    (define (test-help-requested? args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (parser 'help)
      ) ;let
    ) ;define

    (define (main)
      ;; 程序入口点
      (let ((args (command-line)))
        (if (test-help-requested? args)
            (begin (show-help) (exit 0))
            (run-goldtest)
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
