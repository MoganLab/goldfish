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
;; 2. 并行模式逐文件在执行完毕后原子打印 [n/total] path ... PASS/FAIL 进度（即时 flush），
;;    避免多 worker 并发写控制台导致路径与结果被拆散；失败时输出捕获的 stdout/stderr 尾部；
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
    (liii gfproject config)
  ) ;import
  (export ESC color GREEN RED YELLOW RESET goldfish-cmd run-test-file
    display-fail-log cleanup-log run-tests-parallel failed-test-files
    display-summary %dump-captured-tail %report-test-failure %parallel-result
    %process-exit-code %captured-stdout %captured-stderr %print-progress-marker
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

    (define (%tool-domain-inc d test-file)
      (let* ((tdir (cdr (assq 'tests d))) (prefix (string-append tdir "/")))
        (if (not (string-starts? test-file prefix))
          #f
          (let ((root (cdr (assq 'root d))))
            (if (member root '("" "goldfish/" "r7rs/" "library/") string=?)
              ""
              (let ((clean
                      (if (string-ends? root "/") (substring root 0 (- (string-length root) 1)) root)
                    ) ;clean
                   ) ;
                (string-append "-I " clean " ")
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (tool-inc-for-test-file test-file)
      (let loop
        ((ds (gfproject-domains-list)))
        (if (null? ds)
          ""
          (let ((inc (%tool-domain-inc (car ds) test-file)))
            (if inc inc (loop (cdr ds)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (goldfish-cmd . maybe-test-file)
      (let* ((test-file (if (null? maybe-test-file) #f (car maybe-test-file)))
             (inc (if test-file (tool-inc-for-test-file test-file) ""))
            ) ;
        (string-append (executable) " -m r7rs " inc)
      ) ;let*
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
          (let ((code (process-error-code (to-left res))))
            (cons (if (integer? code) code -1) (eqv? code -1))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (run-test-file test-file)
      (let ((cmd (string-append (goldfish-cmd test-file) test-file)))
        (display "----------->\n")
        (display cmd)
        (newline)
        (flush-output-port (current-output-port))
        (let* ((result (%run-test-process cmd 'inherit))
               (code (car result))
               (timeout? (cdr result))
              ) ;
          (if timeout?
            (show #t
              (as-red "FAIL")
              " (timeout "
              (number->string (%test-timeout-seconds))
              "s, killed)\n"
            ) ;show
            (if (zero? code)
              (show #t (as-green "PASS") "\n")
              (show #t (as-red "FAIL") " exit-code=" (number->string code) "\n")
            ) ;if
          ) ;if
          (cons test-file code)
        ) ;let*
      ) ;let
    ) ;define

    (define (display-fail-log tmp-log)
      (when (file-exists? tmp-log)
        (let ((port (open-input-file tmp-log)))
          (let loop
            ()
            (let ((line (read-line port)))
              (unless (eof-object? line)
                (display (string-append "    " line "\n"))
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
                   (let ((cmd (string-append (goldfish-cmd test-file) test-file)))
                     (let ((res (catch #t
                                  (lambda ()
                                    (run-either cmd :stdin 'null :stdout 'capture :stderr 'capture :timeout tmo)
                                  ) ;lambda
                                  (lambda args #f)
                                ) ;catch
                           ) ;res
                          ) ;
                       (%parallel-result idx total res test-file tmo)
                     ) ;let
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

    ;; 打印单文件失败行与捕获输出尾部（导出以便 worker 会话可见）
    (define (%report-test-failure . args)
      (let* ((p5 (= (length args) 5))
             (prefix (if p5 (car args) ""))
             (tmo (if p5 (cadr args) (car args)))
             (code (if p5 (caddr args) (cadr args)))
             (out (if p5 (cadddr args) (caddr args)))
             (err (if p5 (car (cddddr args)) (cadddr args)))
             (reason (if (= code -1)
                       (string-append " (timeout " (number->string tmo) "s, killed)\n")
                       (string-append " exit-code=" (number->string code) "\n")
                     ) ;if
             ) ;reason
            ) ;
        (display (show #f prefix (as-red "FAIL") reason))
        (%dump-captured-tail "stdout" out)
        (%dump-captured-tail "stderr" err)
        (flush-output-port (current-output-port))
      ) ;let*
    ) ;define

    ;; 从 run-either 结果提取退出码（导出以便 worker 会话可见）
    (define (%process-exit-code res)
      (if (either-left? res)
        (let ((c (process-error-code (to-left res))))
          (if (integer? c) c -1)
        ) ;let
        0
      ) ;if
    ) ;define

    ;; 从 run-either 结果提取捕获的 stdout/stderr（导出以便 worker 会话可见）
    (define (%captured-stdout res)
      (if (either-left? res) (process-error-stdout (to-left res)) "")
    ) ;define

    (define (%captured-stderr res)
      (if (either-left? res) (process-error-stderr (to-left res)) "")
    ) ;define

    (define (%progress-prefix idx total test-file)
      (string-append "  ["
        (number->string (+ idx 1))
        "/"
        (number->string total)
        "] "
        test-file
        " ... "
      ) ;string-append
    ) ;define

    ;; 打印 [n/total] file ... 进度标记并立即 flush（导出以便 worker 会话可见）
    (define (%print-progress-marker idx total test-file)
      (display (%progress-prefix idx total test-file))
      (flush-output-port (current-output-port))
    ) ;define

    ;; 并行任务结果处置（导出以便 worker 会话可见）；
    ;; 整体防御性 catch：报告路径出错时降级为 FAIL 结果，
    ;; 不让单个异常炸掉整个 runner（之前 CI 上曾因处理超时结果时
    ;; 的访问错误导致 runner 崩溃，掩盖真实卡点）
    (define (%parallel-result . args)
      (let* ((p5 (= (length args) 5))
             (prefix (if p5
                       (%progress-prefix (car args) (cadr args) (cadddr args))
                       (string-append "  " (cadr args) " ... ")
                     ) ;if
             ) ;prefix
             (res (if p5 (caddr args) (car args)))
             (test-file (if p5 (cadddr args) (cadr args)))
             (tmo (if p5 (car (cddddr args)) (caddr args)))
            ) ;
        (catch #t
          (lambda () (%parallel-result% prefix res test-file tmo))
          (lambda err-args
            (display (show #f
                       prefix
                       (as-red "FAIL")
                       " (report error: "
                       (object->string (car err-args))
                       ")\n"
                     ) ;show
            ) ;display
            (flush-output-port (current-output-port))
            (list test-file -1 "")
          ) ;lambda
        ) ;catch
      ) ;let*
    ) ;define

    (define (%parallel-result% prefix res test-file tmo)
      (if (either-right? res)
        (begin
          (display (show #f prefix (as-green "PASS") "\n"))
          (flush-output-port (current-output-port))
          (list test-file 0 "")
        ) ;begin
        (let ((code (%process-exit-code res)))
          (%report-test-failure prefix
            tmo
            code
            (%captured-stdout res)
            (%captured-stderr res)
          ) ;%report-test-failure
          (list test-file code "")
        ) ;let
      ) ;if
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
