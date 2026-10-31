(import (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii tempfile)
  (liii pi ai log)
) ;import

(check-set-mode! 'report-failed)

;; pi-log-http-error!
;;
;; 异常 HTTP 请求（429/403/5xx/网络错误等）以 ERROR 级别落盘，
;; 并把响应体原文（不做任何转义或截断）原样写入日志。

(define *base* (make-temp-dir "gf-pi-log-http-" ""))

(define *dir* (path-join (path *base*) "logs"))
(pi-log-init! *dir*)

(define *log-file* (pi-log-file-path))

(define (log-content)
  (path-read-text *log-file*)
) ;define

(define raw-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"您已达到每周/每月使用上限（quota 100%），请等待重置\"}}"
) ;define

(pi-log-http-error! 429
  raw-body
  (list (cons 'provider "zai-coding-cn")
    (cons 'model "glm-5.3")
    (cons 'url "POST https://api.z.ai/api/coding/paas/v4/chat/completions")
    (cons 'attempt 1)
    (cons 'max-attempts 3)
    (cons 'retryable #f)
    (cons 'retry-after 2)
  ) ;list
) ;pi-log-http-error!

;; 1. 以 ERROR 级别记录
(check-true (string-contains? (log-content) "[ERROR]"))

;; 2. 记录状态码与请求上下文
(check-true (string-contains? (log-content) "status=429"))
(check-true (string-contains? (log-content) "provider=zai-coding-cn"))
(check-true (string-contains? (log-content) "model=glm-5.3"))
(check-true (string-contains? (log-content) "attempt=1/3"))
(check-true (string-contains? (log-content) "retry-after=2"))
(check-true (string-contains? (log-content)
              "url=POST https://api.z.ai/api/coding/paas/v4/chat/completions"
            ) ;string-contains?
) ;check-true

;; 3. 响应体原样记录（中文、百分号均不转义）
(check-true (string-contains? (log-content) raw-body))

;; 4. 多行响应体逐字节保留

(define multiline-body
  "{\n  \"error\": \"line1\",\n  \"message\": \"第二行\"\n}"
) ;define

(pi-log-http-error! 500 multiline-body '())
(check-true (string-contains? (log-content) multiline-body))

;; 5. 空响应体（网络错误 status=0）也能记录
(pi-log-http-error! 0 "" '())
(check-true (string-contains? (log-content) "status=0"))

;; 6. 额度耗尽类错误附带额度类型
(pi-log-http-error! 429
  "{\"error\":{\"message\":\"您已达到5小时用量上限\"}}"
  (list (cons 'provider "zai-coding-cn"))
) ;pi-log-http-error!
(check-true (string-contains? (log-content) "limit=five-hour"))

;; 7. 关闭开关时不再写入
(putenv "PI_LOG" "off")
(pi-log-reset!)
(pi-log-init! *dir*)
(pi-log-http-error! 502 "should not be written" '())
(check (string-contains? (log-content) "should not be written") => #f)
(unsetenv "PI_LOG")

(pi-log-reset!)
(path-unlink *log-file*)
(path-rmdir *dir*)
(path-rmdir *base*)

(check-report)
