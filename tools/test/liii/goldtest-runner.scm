;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;; 测试运行器：每个测试文件在独立子进程中执行（stdin 接 null，超时默认
;; 300s，GOLDFISH_TEST_TIMEOUT 可覆盖）；并行模式逐文件原子打印进度，失败
;; 时输出捕获尾部；并行闭包经 (liii par) 跨 worker 会话执行，自由符号必须
;; 是主库导出绑定。
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
    (liii goldtest-project)
    (liii goldtest-cache)
    (liii goldtest-cache-deps)
    (liii goldtest-runner-report)
  ) ;import
  (export ESC color GREEN RED YELLOW RESET goldfish-cmd run-test-file
    run-tests-parallel %parallel-result %process-exit-code %captured-stdout
    %captured-stderr %print-progress-marker *test-cache-force*
    set-test-cache-force! test-cache-force? %cache-check %cache-record
    %parallel-cached-result %run-one-parallel-test %build-indexed-vec
    display-fail-log cleanup-log failed-test-files display-summary
    %dump-captured-tail %report-test-failure cached-count-bump!
    cached-count-reset! %count-cached-results
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

    ;; 缓存控制：由入口在运行前通过 set-test-cache-force! 设置（#t 表示强制绕过缓存）
    (define *test-cache-force* #f)

    ;; 设置缓存强制绕过标志（库内部变量，须由库内 setter 修改）
    (define (set-test-cache-force! flag)
      (set! *test-cache-force* (and flag #t))
    ) ;define

    ;; 查询当前缓存强制绕过标志
    (define (test-cache-force?)
      *test-cache-force*
    ) ;define

    ;; 缓存按实际 domain 执行参数计算，聚合运行与单 domain 运行保持一致。
    (define (%test-cache-key test-file)
      (test-action-key test-file (caddr (%test-execution-context test-file)))
    ) ;define

    ;; 检查缓存是否命中（未启用或强制绕过时返回 #f；force-flag 参数化传入 worker）
    (define (%cache-check test-file . maybe-force)
      (let ((force-flag (if (pair? maybe-force) (car maybe-force) *test-cache-force*)))
        (catch #t
          (lambda ()
            (and (not force-flag)
              (file-exists? test-file)
              (not (test-no-cache-directive? test-file))
              (let ((key (%test-cache-key test-file)))
                (and (test-cache-hit? (test-cache-path key)) key)
              ) ;let
            ) ;and
          ) ;lambda
          (lambda args #f)
        ) ;catch
      ) ;let
    ) ;define

    ;; 测试成功后写回缓存（force 模式下成功后仍刷新缓存）
    (define (%cache-record test-file exit-code . maybe-force)
      (catch #t
        (lambda ()
          (when (and (file-exists? test-file)
                  (not (test-no-cache-directive? test-file))
                  (zero? exit-code)
                ) ;and
            (let ((key (%test-cache-key test-file)))
              (test-cache-record! (test-cache-path key) exit-code)
            ) ;let
          ) ;when
        ) ;lambda
        (lambda args #f)
      ) ;catch
    ) ;define

    (define (goldfish-cmd . maybe-test-file)
      (let ((test-file (and (pair? maybe-test-file) (car maybe-test-file))))
        (string-append (executable) (%goldfish-cmd-flags test-file))
      ) ;let
    ) ;define

    ;; --all 从项目根目录聚合所有 domain，但测试的执行约定与单独运行该
    ;; domain 相同：cwd 为 domain root，路径为 root 下的 tests/...。
    (define (%test-execution-context test-file)
      (let* ((project-root (%find-project-root (getcwd)))
             (domain (%find-domain-for-test-file test-file))
             (root
               (and domain (string-trim-right (cdr (assq 'root domain)) #\/))
             ) ;root
             (tests-prefix
               (and domain (string-append (cdr (assq 'tests domain)) "/"))
             ) ;tests-prefix
             (local-file (if (and tests-prefix (string-starts? test-file tests-prefix))
                           (substring test-file (string-length tests-prefix))
                           test-file
                         ) ;if
             ) ;local-file
            ) ;
        (if (and domain project-root root (> (string-length root) 0))
          (let* ((cwd (path->string (path-join project-root root)))
                 (run-file (if (string-starts? test-file "tests/")
                             test-file
                             (string-append "tests/" local-file)
                           ) ;if
                 ) ;run-file
                ) ;
            (list cwd run-file (%goldfish-cmd-flags run-file domain))
          ) ;let*
          (list #f test-file (%goldfish-cmd-flags test-file domain))
        ) ;if
      ) ;let*
    ) ;define

    (define (%test-command test-file)
      (let ((context (%test-execution-context test-file)))
        (cons (string-append (executable) (caddr context) (cadr context)) context)
      ) ;let
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
    (define (%run-test-process cmd stdout-mode cwd)
      (let ((res
              (catch #t
                (lambda ()
                  (run-either cmd
                    :cwd     cwd
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
      (let ((cache-key (%cache-check test-file)))
        (if cache-key
          (begin
            ;; 命中缓存：静默跳过，仅在 Summary 汇总命中数
            (cached-count-bump!)
            (cons test-file 0)
          ) ;begin
          (let* ((command (%test-command test-file))
                 (cmd (car command))
                 (context (cdr command))
                ) ;
            (display "----------->\n")
            (display cmd)
            (newline)
            (flush-output-port (current-output-port))
            (let* ((result (%run-test-process cmd 'inherit (car context)))
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
              (%cache-record test-file code)
              (cons test-file code)
            ) ;let*
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 构造 (idx file) 索引化任务向量
    (define (%build-indexed-vec test-files)
      (list->vector
        (let loop
          ((i 0) (rest test-files))
          (if (null? rest) '() (cons (list i (car rest)) (loop (+ i 1) (cdr rest))))
        ) ;let
      ) ;list->vector
    ) ;define

    ;; 单个并行任务体（导出以便 worker 会话可见）：
    ;; 先查缓存，命中静默返回（不打印，仅在 Summary 汇总），否则拉起子进程执行
    (define (%run-one-parallel-test pair total tmo cache-force)
      (let* ((idx (car pair))
             (test-file (cadr pair))
             (cached (%cache-check test-file cache-force))
            ) ;
        (if cached
          (%parallel-cached-result idx total test-file)
          (let* ((command (%test-command test-file))
                 (cmd (car command))
                 (context (cdr command))
                ) ;
            (let ((res
                    (catch #t
                      (lambda ()
                        (run-either cmd
                          :cwd     (car context)
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
              (%parallel-result idx total res test-file tmo cache-force)
            ) ;let
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    ;; 并行执行：逐文件即时打印进度与失败诊断。
    ;; 闭包在 (liii go) worker 会话内执行，只能引用主库导出绑定；
    ;; tmo/cache-force 等参数经 funclet 捕获环境以可序列化值传入
    (define (run-tests-parallel test-files jobs)
      (let* ((total (length test-files))
             (exe (executable))
             (tmo (%test-timeout-seconds))
             (cache-force (test-cache-force?))
             (vec (%build-indexed-vec test-files))
             (run-one-test (lambda (pair) (%run-one-parallel-test pair total tmo cache-force))
             ) ;run-one-test
             (raw-results (vector-par-map run-one-test vec jobs))
            ) ;
        ;; 主会话收集结果后统一累计缓存命中数（worker 侧库变量不可见）
        (let ((n-cached (%count-cached-results (vector->list raw-results))))
          (let loop
            ((i 0))
            (when (< i n-cached)
              (cached-count-bump!)
              (loop (+ i 1))
            ) ;when
          ) ;let
        ) ;let
        (vector->list
          (vector-map (lambda (r) (cons (car r) (cadr r))) raw-results)
        ) ;vector->list
      ) ;let*
    ) ;define

    ;; 缓存命中结果标记（命中时不打印单行，仅在 Summary 汇总命中数）
    (define (%parallel-cached-result idx total test-file)
      (list test-file 0 "cached")
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
      (let* ((p5 (>= (length args) 5))
             (prefix (if p5
                       (%progress-prefix (car args) (cadr args) (cadddr args))
                       (string-append "  " (cadr args) " ... ")
                     ) ;if
             ) ;prefix
             (res (if p5 (caddr args) (car args)))
             (test-file (if p5 (cadddr args) (cadr args)))
             (tmo (if p5 (car (cddddr args)) (caddr args)))
             (force-flag
               (if (and p5 (> (length args) 5)) (list-ref args 5) #f)
             ) ;force-flag
            ) ;
        (catch #t
          (lambda () (%parallel-result% prefix res test-file tmo force-flag))
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

    (define (%parallel-result% prefix res test-file tmo . maybe-force)
      (let ((force-flag (if (pair? maybe-force) (car maybe-force) #f)))
        (if (either-right? res)
          (begin
            (display (show #f prefix (as-green "PASS") "\n"))
            (flush-output-port (current-output-port))
            (%cache-record test-file 0 force-flag)
            (list test-file 0 "")
          ) ;begin
          (let ((code (%process-exit-code res)))
            (%report-test-failure prefix
              tmo
              code
              (%captured-stdout res)
              (%captured-stderr res)
            ) ;%report-test-failure
            (%cache-record test-file code force-flag)
            (list test-file code "")
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
