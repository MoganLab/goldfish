;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 独立脚本模式 ping；不读写主会话，不加载工具与技能。
(define-library (liii pi code all-in-one probe)
  (import (scheme base)
    (scheme char)
    (scheme process-context)
    (liii either)
    (liii list)
    (liii path)
    (liii string)
    (liii subprocess)
    (liii sys)
    (liii pi code all-in-one alarm)
    (liii pi code all-in-one types)
    (liii pi code config)
  ) ;import
  (export pi-probe-node pi-probe-prompt pi-probe-error-code)
  (begin

    (define pi-probe-prompt "ping")

    (define (%http-code message start)
      (let loop
        ((i start) (digits '()))
        (if (and (< i (string-length message)) (char-numeric? (string-ref message i)))
          (loop (+ i 1) (cons (string-ref message i) digits))
          (if (pair? digits) (string->number (list->string (reverse digits))) 'error)
        ) ;if
      ) ;let
    ) ;define

    ;; 从子进程错误文本提取 HTTP 状态码；无状态码时保留超时类别。
    (define (pi-probe-error-code message)
      (let ((pos (and (string? message) (string-position "HTTP " message))))
        (cond (pos (%http-code message (+ pos 5)))
              ((and (string? message) (string-contains? (string-downcase message) "timeout"))
               'timeout
              ) ;
              (else 'error)
        ) ;cond
      ) ;let
    ) ;define

    (define (%config-env config-dir)
      (cons (cons "GF_CONFIG_DIR" config-dir)
        (filter
          (lambda (entry) (not (equal? (car entry) "GF_CONFIG_DIR")))
          (get-environment-variables)
        ) ;filter
      ) ;cons
    ) ;define

    (define (%probe-command node)
      (list (executable)
        "pi"
        "-p"
        "--no-session"
        "--no-tools"
        "--no-skills"
        "--provider"
        (chain-node-provider node)
        "--model"
        (chain-node-model node)
        "--thinking"
        (chain-node-thinking-level node)
        pi-probe-prompt
      ) ;list
    ) ;define

    (define (%default-run command timeout-ms config-dir)
      (let ((result (run-either command
                      :env     (%config-env config-dir)
                      :stdin   'null
                      :stdout  'capture
                      :stderr  'capture
                      :timeout (/ timeout-ms 1000)
                    ) ;run-either
            ) ;result
           ) ;
        (if (either-right? result)
          (values 0 (to-right result) "")
          (let ((error (to-left result)))
            (values (process-error-code error)
              (process-error-stdout error)
              (process-error-stderr error)
            ) ;values
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%failure code message)
      (list code message)
    ) ;define

    (define (%probe-failure exit-code stdout stderr)
      (let ((message
              (if (and (string? stderr) (not (string-null? stderr))) stderr stdout)
            ) ;message
           ) ;
        (cond
         ((eqv? exit-code -1)
          (%failure 'timeout (if (string-null? message) "probe process timeout" message))
         ) ;
         ((not (zero? exit-code)) (%failure (pi-probe-error-code message) message))
         ((and (string? stdout) (not (string-null? (string-trim-both stdout)))) #f)
         (else (%failure 'empty-response "probe returned no text"))
        ) ;cond
      ) ;let
    ) ;define

    ;; run-fn 接收 command、timeout-ms、config-dir，返回退出码与两路输出。
    ;; 正常文本返回 #t；任何错误统一写 FAIL 日志并返回 #f。
    (define* (pi-probe-node node (timeout-ms 8000) (config-dir #f) (run-fn #f))
      (let* ((provider (chain-node-provider node))
             (model (chain-node-model node))
             (dir (path->string (or config-dir (gf-get-config-dir))))
             (run-probe (or run-fn %default-run))
             (failure
               (catch #t
                 (lambda ()
                   (let-values (((exit-code stdout stderr) (run-probe (%probe-command node) timeout-ms dir)))
                     (%probe-failure exit-code stdout stderr)
                   ) ;let-values
                 ) ;lambda
                 (lambda (tag . args)
                   (%failure 'exception
                     (if (pair? args)
                       (let ((detail (car args)))
                         (if (string? detail) detail (object->string detail))
                       ) ;let
                       (object->string tag)
                     ) ;if
                   ) ;%failure
                 ) ;lambda
               ) ;catch
             ) ;failure
            ) ;
        (when failure
          (pi-alarm-log! "FAIL" provider model (car failure) (cadr failure) dir)
        ) ;when
        (not failure)
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
