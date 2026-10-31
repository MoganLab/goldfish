(import (scheme base)
  (liii check)
  (liii string)
  (liii subprocess)
  (liii os)
  (liii path)
  (liii tempfile)
  (liii pi ai message)
  (liii pi ai log)
  (liii pi ai api openai-completions)
) ;import

(check-set-mode! 'report-failed)

;; 异常请求（429）写入 ~/.gf/agent/logs，测试重定向到临时目录

(define *log-base* (make-temp-dir "gf-pi-quota-log-" ""))

(define *log-dir* (path-join (path *log-base*) "logs"))
(pi-log-init! *log-dir*)

(define *log-file* (pi-log-file-path))

;; 额度耗尽（HTTP 429）端到端测试
;;
;; Z.AI Coding Plan 在额度耗尽时返回 429，区分 5 小时窗口额度与每周/每月额度。
;; 这类错误属于账户级限额，重试无意义：pi 应当立即失败并给出额度类型与重置时间。

(define (find-stub-server)
  (let loop
    ((candidates '("tools/pi/tests/stub_quota_server.py"
                   "tests/stub_quota_server.py" "stub_quota_server.py"
                   "../stub_quota_server.py" "../../stub_quota_server.py"
                   "../../../stub_quota_server.py"
                   "../../../../tools/pi/tests/stub_quota_server.py"
                   "../../../../../tools/pi/tests/stub_quota_server.py")
     ) ;candidates
    ) ;
    (cond ((null? candidates) (error 'file-not-found "stub_quota_server.py not found"))
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates)))
    ) ;cond
  ) ;let
) ;define

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define (count-lines path)
  (if (file-exists? path)
    (let ((port (open-input-file path)))
      (let loop
        ((n 0) (ch (read-char port)))
        (if (eof-object? ch)
          (begin
            (close-input-port port)
            n
          ) ;begin
          (loop (if (char=? ch #\newline) (+ n 1) n) (read-char port))
        ) ;if
      ) ;let
    ) ;let
    0
  ) ;if
) ;define

(define (make-log-path)
  (let*-values (((port path) (make-temp-file "gf-pi-quota" ".log")))
    (close-output-port port)
    path
  ) ;let*-values
) ;define

(define (make-config base-url)
  (list (cons 'provider "zai-coding-cn")
    (cons 'model-id "glm-5.3")
    (cons 'model-meta `(("base-url" . ,base-url) ("max-tokens" . 1024)))
    (cons 'api-key "dummy-key")
    (cons 'proxy-url #f)
    (cons 'tools '())
    (cons 'settings
      '((retry (enabled . #t)
          (max-retries . 5)
          (base-delay-ms . 10)
          (provider (max-retries . 5))))
    ) ;cons
  ) ;list
) ;define

(define (with-stub logfile body proc)
  (catch #t (lambda () (remove logfile)) (lambda args #f))
  (let* ((child (process-spawn (list "python3" (find-stub-server) body logfile) :stdout 'pipe)
         ) ;child
         (port (parse-port-line (process-read-line child)))
         (base-url (string-append "http://127.0.0.1:" (number->string port)))
        ) ;
    (if (not (number? port))
      (error 'file-not-found "stub quota server did not report a port")
      (let ((result (proc base-url)))
        (process-kill child)
        result
      ) ;let
    ) ;if
  ) ;let*
) ;define

(define weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限，您的限额将在 2026-11-05 13:12:35 重置。\"}}"
) ;define

(define five-hour-body
  "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时使用上限，请在 5 小时后重试。\"}}"
) ;define

;; 1. 流式请求：周额度耗尽 → 立即失败，提示额度类型与重置时间

(define weekly-log (make-log-path))

(with-stub weekly-log
  weekly-body
  (lambda (base-url)
    (let ((resp (openai-request-stream (make-config base-url) (list (pi-user-message "ping")))
          ) ;resp
         ) ;
      (check (pi-assistant-message-stop-reason resp) => "error")
      (let ((msg (pi-assistant-message-error-message resp)))
        (check-true (string-contains? msg "您已达到每周/每月使用上限"))
        (check-true (string-contains? msg "2026-11-05 13:12:35"))
      ) ;let
      resp
    ) ;let
  ) ;lambda
) ;with-stub

;; 2. 非流式请求：5 小时额度耗尽 → 同样立即失败

(define five-hour-log (make-log-path))

(with-stub five-hour-log
  five-hour-body
  (lambda (base-url)
    (let ((resp (openai-request (make-config base-url)
                  (list (pi-user-message "ping"))
                  #f
                  '()
                  10
                ) ;openai-request
          ) ;resp
         ) ;
      (check (pi-assistant-message-stop-reason resp) => "error")
      (let ((msg (pi-assistant-message-error-message resp)))
        (check-true (string-contains? msg "您已达到5小时使用上限"))
      ) ;let
      resp
    ) ;let
  ) ;lambda
) ;with-stub

;; 3. 额度耗尽属于确定性失败：不应触发任何重试（stub server 只收到 1 次请求）
(check (count-lines weekly-log) => 1)
(check (count-lines five-hour-log) => 1)

;; 4. 异常请求（429）以 ERROR 级别落盘，响应体原文原样保留

(define log-text (path-read-text *log-file*))

(check-true (string-contains? log-text "[ERROR]"))
(check-true (string-contains? log-text "status=429"))
(check-true (string-contains? log-text "provider=zai-coding-cn"))
(check-true (string-contains? log-text "model=glm-5.3"))
(check-true (string-contains? log-text "limit=weekly"))
(check-true (string-contains? log-text "您已达到每周/每月使用上限"))
(check-true (string-contains? log-text "您已达到5小时使用上限"))
(check-true (string-contains? log-text "retryable=no"))

(catch #t (lambda () (remove weekly-log)) (lambda args #f))
(catch #t (lambda () (remove five-hour-log)) (lambda args #f))

;; 清理日志临时目录
(pi-log-reset!)
(path-unlink *log-file*)
(path-rmdir *log-dir*)
(path-rmdir *log-base*)

(check-report)
