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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; 测试报告渲染：失败日志回显、失败行打印、捕获输出尾部转储与
;; Summary 汇总统计（含 cached 命中数展示）。
;; 注意：%report-test-failure 会经 (liii par) 跨 (liii go) worker
;; 会话执行，其自由符号必须是本库或主库导出绑定。

(define-library (liii goldtest-runner-report)
  (import (scheme base)
    (scheme file)
    (liii os)
    (liii list)
    (liii string)
    (liii format base)
    (liii format color)
  ) ;import
  (export display-fail-log cleanup-log failed-test-files display-summary
    cached-count-bump! cached-count-reset! %dump-captured-tail
    %report-test-failure %count-cached-results %shorten-home %runnable-line
    %rerun-test-path %progress-prefix %print-progress-marker %build-indexed-vec
  ) ;export
  (begin

    ;; 缓存命中统计（主会话使用；worker 侧命中数经结果列表统计）
    (define *cached-count* 0)

    (define (cached-count-reset!)
      (set! *cached-count* 0)
    ) ;define

    (define (cached-count-bump!)
      (set! *cached-count* (+ *cached-count* 1))
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
          (delete-file tmp-log)
        ) ;let
      ) ;when
    ) ;define

    (define (cleanup-log tmp-log)
      (when (file-exists? tmp-log)
        (delete-file tmp-log)
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
        (if (> *cached-count* 0)
          (show #t
            "  "
            (as-green "Passed: " (number->string passed))
            " ("
            (number->string *cached-count*)
            " cached)\n"
          ) ;show
          (show #t "  " (as-green "Passed: " (number->string passed)) "\n")
        ) ;if
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

    ;; 统计缓存命中次数：由主会话在收集并行结果后调用
    (define (%count-cached-results raw-results)
      (count
        (lambda (r)
          (and (pair? r) (= (length r) 3) (equal? (caddr r) "cached") (zero? (cadr r)))
        ) ;lambda
        raw-results
      ) ;count
    ) ;define

    ;; 打印单文件失败行与捕获输出尾部（导出以便 worker 会话可见）
    ;; verbose? 为 #t 时不截断输出尾部；rerun 为可直接复制的重跑命令
    (define (%report-test-failure . args)
      (let* ((n (length args))
             (verbose? (and (>= n 6) (list-ref args 5)))
             (rerun (and (>= n 6) (list-ref args 6)))
             (base (if (>= n 6) (take args 5) args))
             (p5 (= (length base) 5))
             (prefix (if p5 (car base) ""))
             (tmo (if p5 (cadr base) (car base)))
             (code (if p5 (caddr base) (cadr base)))
             (out (if p5 (cadddr base) (caddr base)))
             (err (if p5 (car (cddddr base)) (cadddr base)))
             (reason (if (= code -1)
                       (string-append " (timeout " (number->string tmo) "s, killed)\n")
                       (string-append " exit-code=" (number->string code) "\n")
                     ) ;if
             ) ;reason
            ) ;
        (display (show #f prefix (as-red "FAIL") reason))
        (%dump-captured-tail "stdout" out verbose? rerun)
        (%dump-captured-tail "stderr" err verbose? rerun)
        (flush-output-port (current-output-port))
      ) ;let*
    ) ;define

    ;; 打印捕获文本的末尾若干行（供失败诊断；导出以便 worker 会话可见）
    ;; 默认只显示末尾 20 行；verbose? 为 #t 时不截断。发生截断且提供
    ;; rerun 命令时，提示可复制重跑以查看完整输出。
    (define (%dump-captured-tail label text . opts)
      (let ((verbose? (and (pair? opts) (car opts)))
            (rerun
              (if (and (pair? opts) (> (length opts) 1)) (cadr opts) #f)
            ) ;rerun
           ) ;
        (when (and (string? text) (> (string-length text) 0))
          (let* ((lines (reverse (string-split text "\n")))
                 (limit (if verbose? 0 20))
                 (acc
                   (let loop
                     ((rest lines) (acc '()) (n 0))
                     (if (or (not (pair? rest)) (and (> limit 0) (>= n limit)))
                       acc
                       (loop (cdr rest) (cons (car rest) acc) (+ n 1))
                     ) ;if
                   ) ;let
                 ) ;acc
                ) ;
            (display (string-append "    -- "
                       label
                       " (last "
                       (number->string (length acc))
                       " lines) --"
                     ) ;string-append
            ) ;display
            (newline)
            (for-each (lambda (l) (display "    ") (display l) (newline)) acc)
            (when (and rerun (not verbose?) (> (length lines) (length acc)))
              (display "    (output truncated; rerun to see all lines: ")
              (display rerun)
              (display ")")
              (newline)
            ) ;when
          ) ;let*
        ) ;when
      ) ;let
    ) ;define

    ;; 将路径中的家目录前缀压缩为 ~（便于复制命令行；不使用 $ 变量）
    (define (%shorten-home path)
      (let ((home (getenv "HOME")))
        (if
          (and home
            (> (string-length home) 0)
            (or (string=? path home) (string-starts? path (string-append home "/")))
          ) ;and
          (string-append "~" (substring path (string-length home)))
          path
        ) ;if
      ) ;let
    ) ;define

    ;; 生成可直接复制运行的单行命令：cd <cwd> && <cmd>（cwd 用 ~ 缩写）
    (define (%runnable-line cwd cmd)
      (if (and (string? cwd) (> (string-length cwd) 0))
        (string-append "cd " (%shorten-home cwd) " && " cmd)
        cmd
      ) ;if
    ) ;define

    ;; 重跑路径：cwd 相对 project-root 的域目录 + tests 下相对路径；
    ;; 无法推导时回退原始 test-file
    (define (%rerun-test-path project-root cwd run-file fallback)
      (let* ((cwd-norm (string-replace (string-trim-right cwd #\/) "\\" "/"))
             (prefix (string-append project-root "/"))
            ) ;
        (if (and (string-starts? cwd-norm prefix)
              (> (string-length cwd-norm) (string-length prefix))
            ) ;and
          (string-append (substring cwd-norm (string-length prefix)) "/" run-file)
          fallback
        ) ;if
      ) ;let*
    ) ;define

    ;; 并行模式单文件进度前缀：  [idx/total] file ...
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

    (define (%print-progress-marker idx total test-file)
      (display (%progress-prefix idx total test-file))
      (flush-output-port (current-output-port))
    ) ;define

    ;; 索引化任务向量：元素为 (list idx file)，供并行闭包携带序号
    (define (%build-indexed-vec test-files)
      (list->vector
        (let loop
          ((i 0) (rest test-files))
          (if (null? rest) '() (cons (list i (car rest)) (loop (+ i 1) (cdr rest))))
        ) ;let
      ) ;list->vector
    ) ;define

  ) ;begin
) ;define-library
