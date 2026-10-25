;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; 测试运行器：
;; 1. 每个测试文件在独立子进程中执行，stdin 接 null（子进程内脚本错误
;;    掉进 REPL 时立即读到 EOF 退出，而不是阻塞在等输入卡死 CI），
;;    并施加超时（默认 300s，环境变量 GOLDFISH_TEST_TIMEOUT 可覆盖），
;;    超时强杀子进程并记为 FAIL，单文件挂起不再拖死整个测试步骤；
;; 2. 并行模式逐文件打印 [n/total] RUNNING/PASS/FAIL 进度（即时 flush），
;;    失败时输出捕获的 stdout/stderr 尾部，方便定位卡点；
;; 注意：并行闭包经 (liii par) 跨 (liii go) worker 会话执行，其自由符号
;; 必须是主库导出绑定（display/run-either 等），不能引用本库内部函数。
(define-library (liii goldtest-runner)
  (import (scheme base)
    (scheme file)
    (liii list)
    (liii os)
    (liii path)
    (liii sys)
    (liii par)
    (liii string)
    (liii either)
    (liii subprocess)
    (liii format base)
    (liii format color)
  ) ;import
  (export ESC color GREEN RED YELLOW RESET goldfish-cmd run-test-file
    display-fail-log cleanup-log run-tests-parallel failed-test-files
    display-summary %dump-captured-tail
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

    (define (goldfish-cmd)
      (string-append (executable) " -m r7rs ")
    ) ;define

    ;; 单个测试文件超时秒数：GOLDFISH_TEST_TIMEOUT 环境变量可覆盖
    (define (%test-timeout-seconds)
      (let ((env (getenv "GOLDFISH_TEST_TIMEOUT")))
        (let ((n (and env (string->number env))))
          (if (and n (real? n) (>= n 1)) (exact (floor n)) 300)
        ) ;let
      ) ;let
    ) ;define

    ;; 在主会话执行单个测试子进程（串行模式用，stdout 实时透传）：
    ;; 返回 (exit-code . timeout?)；timeout? 为 #t 表示被超时强杀
    (define (%run-test-process cmd stdout-mode)
      (let ((res
              (catch #t
                (lambda ()
                  (run-either cmd
                    :stdin   'null
                    :stdout  stdout-mode
                    :stderr  'stdout
                    :timeout (%test-timeout-seconds)
                  ) ;run-either
                ) ;lambda
                (lambda args (from-left (make-process-error -1 "" "" cmd)))
              ) ;catch
            ) ;res
           ) ;
        (if (either-right? res)
          (cons 0 #f)
          (let ((code (process-error-code res)))
            (cons (if (integer? code) code -1) (eqv? code -1))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (run-test-file test-file)
      (let ((cmd (string-append (goldfish-cmd) test-file)))
        (display "----------->")
        (newline)
        (display cmd)
        (newline)
        (flush-output-port (current-output-port))
        (let ((result (%run-test-process cmd 'inherit)))
          (if (= (cdr result) -1)
            (show #t
              (as-red "FAIL")
              " (timeout "
              (number->string (%test-timeout-seconds))
              "s, killed)"
              "\n"
            ) ;show
            (if (zero? (cdr result))
              (show #t (as-green "PASS") "\n")
              (show #t (as-red "FAIL") " exit-code=" (number->string (cdr result)) "\n")
            ) ;if
          ) ;if
          (cons test-file (car result))
        ) ;let
      ) ;let
    ) ;define

    (define (display-fail-log tmp-log)
      (when (file-exists? tmp-log)
        (let ((port (open-input-file tmp-log)))
          (let loop
            ()
            (let ((line (read-line port)))
              (unless (eof-object? line)
                (display "    ")
                (display line)
                (newline)
                (loop)
              ) ;unless
            ) ;let
          ) ;let
          (close-input-port port)
          (remove tmp-log)
        ) ;let
      ) ;when
    ) ;define

    (define (cleanup-log tmp-log)
      (when (file-exists? tmp-log)
        (remove tmp-log)
      ) ;when
    ) ;define

    (define (failed-test-files test-results)
      (map car
        (filter
          (lambda (test-result) (not (zero? (cdr test-result))))
          test-results
        ) ;filter
      ) ;map
    ) ;define

    ;; 汇总（逐文件结果已在运行时即时打印，此处只输出统计与失败清单）
    (define (display-summary test-results)
      (let* ((total (length test-results))
             (passed
               (count (lambda (x) (zero? (cdr x))) test-results)
             ) ;passed
             (failed-files (failed-test-files test-results))
             (failed (- total passed))
            ) ;
        (newline)
        (display "=== Summary ===")
        (newline)
        (display (string-append "  Total:  " (number->string total)))
        (newline)
        (show #t "  " (as-green "Passed: " (number->string passed)) "\n")
        (when (> failed 0)
          (show #t "  " (as-red "Failed: " (number->string failed)) "\n")
          (display "  Failed Test Files:")
          (newline)
          (for-each (lambda (test-file) (display (string-append "    " test-file)) (newline))
            failed-files
          ) ;for-each
        ) ;when
        failed
      ) ;let*
    ) ;define

    ;; 并行执行：逐文件即时打印进度与失败诊断。
    ;; 闭包在 (liii go) worker 会话内执行，只能引用主库导出绑定；
    ;; tmo 等参数经 funclet 捕获环境以可序列化值传入
    (define (run-tests-parallel test-files jobs)
      (let* ((total (length test-files))
             (exe (executable))
             (tmo (%test-timeout-seconds))
             (vec
               (list->vector
                 (let loop
                   ((i 0) (rest test-files))
                   (if (null? rest) '() (cons (list i (car rest)) (loop (+ i 1) (cdr rest))))
                 ) ;let
               ) ;list->vector
             ) ;vec
             (run-one-test
               (lambda (pair)
                 (let ((idx (car pair)) (test-file (cadr pair)))
                   (display (string-append "  ["
                              (number->string (+ idx 1))
                              "/"
                              (number->string total)
                              "] "
                              test-file
                              " ... "
                            ) ;string-append
                   ) ;display
                   (flush-output-port (current-output-port))
                   (let ((res
                           (catch #t
                             (lambda ()
                               (run-either (string-append exe " -m r7rs " test-file)
                                 :stdin   'null
                                 :stdout  'capture
                                 :stderr  'capture
                                 :timeout tmo
                               ) ;run-either
                             ) ;lambda
                             (lambda args #f)
                           ) ;catch
                         ) ;res
                        ) ;
                     (if (either-right? res)
                       (begin
                         (show #t (as-green "PASS") "\n")
                         (list test-file 0 "")
                       ) ;begin
                       (let* ((code
                                (if (either-left? res)
                                  (let ((c (process-error-code res)))
                                    (if (integer? c) c -1)
                                  ) ;let
                                  -1
                                ) ;if
                              ) ;code
                              (out (if (either-left? res) (process-error-stdout res) ""))
                              (err (if (either-left? res) (process-error-stderr res) ""))
                             ) ;
                         (if (= code -1)
                           (show #t (as-red "FAIL") " (timeout " (number->string tmo) "s, killed)" "\n")
                           (show #t (as-red "FAIL") " exit-code=" (number->string code) "\n")
                         ) ;if
                         ;; 失败时打印捕获输出的尾部，帮助定位卡点
                         (%dump-captured-tail "stdout" out)
                         (%dump-captured-tail "stderr" err)
                         (list test-file code "")
                       ) ;let*
                     ) ;if
                   ) ;let
                 ) ;let
               ) ;lambda
             ) ;run-one-test
             (raw-results (vector-par-map run-one-test vec jobs))
            ) ;
        (vector->list
          (vector-map (lambda (r) (cons (car r) (cadr r))) raw-results)
        ) ;vector->list
      ) ;let*
    ) ;define

    ;; 打印捕获文本的末尾若干行（供失败诊断；导出以便 worker 会话可见）
    (define (%dump-captured-tail label text)
      (when (and (string? text) (> (string-length text) 0))
        (let ((lines (string-split text "\n")))
          (let loop
            ((rest (reverse lines)) (acc '()) (n 0))
            (if (or (not (pair? rest)) (>= n 20))
              (begin
                (display (string-append "    -- "
                           label
                           " (last "
                           (number->string (length acc))
                           " lines) --"
                         ) ;string-append
                ) ;display
                (newline)
                (for-each (lambda (l) (display "    ") (display l) (newline)) acc)
              ) ;begin
              (loop (cdr rest) (cons (car rest) acc) (+ n 1))
            ) ;if
          ) ;let
        ) ;let
      ) ;when
    ) ;define

  ) ;begin
) ;define-library
