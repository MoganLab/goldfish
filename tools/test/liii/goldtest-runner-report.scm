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
    (liii list)
    (liii string)
    (liii format base)
    (liii format color)
  ) ;import
  (export display-fail-log cleanup-log failed-test-files display-summary
    cached-count-bump! cached-count-reset! %dump-captured-tail
    %report-test-failure %count-cached-results
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
