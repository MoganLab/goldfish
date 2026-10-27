(import (liii check)
  (liii string)
  (liii subprocess)
  (liii pi ai message)
  (liii pi code core compaction summary)
) ;import

(check-set-mode! 'report-failed)

;; pi-generate-summary
;; 请求模型将会话消息压缩为摘要，并返回摘要文本及 token 用量。
;;
;; 语法
;; ----
;; (pi-generate-summary config messages previous-summary [timeout]) -> alist
;;
;; 参数
;; ----
;; config : alist
;; 包含 provider、api-key、model-id 和 model-meta 的模型配置。
;; messages : list
;; 待总结的消息列表。
;; previous-summary : string | #f
;; 可选的上一轮摘要，用于保持压缩上下文连续。
;; timeout : number
;; 可选请求超时时间，默认 600 秒。
;;
;; 返回值
;; ----
;; alist
;; 包含 summary 和 usage 两项。模型请求失败、返回空摘要或尝试调用工具时，
;; 抛出 summarization-error。
;;
;; 示例
;; ----
;; (pi-generate-summary config messages #f 30)
;; => ((summary . "摘要文本") (usage . ...))

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define stub-server-file
  (if (file-exists? "tests/stub_gemini_server.py")
    "tests/stub_gemini_server.py"
    "tools/pi/tests/stub_gemini_server.py"
  ) ;if
) ;define

(define (make-config base-url)
  (list (cons 'provider "google")
    (cons 'api-key "test-stub-key")
    (cons 'model-id "gemini-3.8-flash")
    (cons 'model-meta (list (cons 'base-url base-url) (cons 'max-tokens 4096)))
    (cons 'proxy-url #f)
    (cons 'timeout 10)
  ) ;list
) ;define

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port (parse-port-line (process-read-line proc)))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
      ) ;
  (check-true (number? port))
  (let ((result (pi-generate-summary (make-config base-url)
                  (list (pi-user-message "Summarize this conversation"))
                  #f
                  10
                ) ;pi-generate-summary
        ) ;result
       ) ;
    (check (cdr (assoc 'summary result)) => "Hello stream!")
    (check-true (pair? (assoc 'usage result)))
  ) ;let
  (check-catch 'summarization-error
    (pi-generate-summary (make-config (string-append base-url "/function-call"))
      (list (pi-user-message "Summarize without tools"))
      #f
      10
    ) ;pi-generate-summary
  ) ;check-catch
  (process-kill proc)
) ;let*

(check-report)
