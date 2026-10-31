(import (liii check)
  (liii string)
  (liii os)
  (liii path)
  (liii tempfile)
  (liii pi ai log)
) ;import

(check-set-mode! 'report-failed)

;; pi-log-http-error!
;; 记录一次异常 HTTP 请求（含响应体原文）。
;;
;; 语法
;; ----
;; (pi-log-http-error! status body info)
;;
;; 参数
;; ----
;; status : integer?
;; HTTP 状态码；网络错误为 0。
;;
;; body : string?
;; 响应体原文（JSON 或 SSE 文本），逐字节写入，不转义、不截断。
;;
;; info : list?
;; 可选元信息关联表，支持 provider、model、url、attempt、max-attempts、
;; retryable、retry-after；额度耗尽时自动附带 limit 字段。
;;
;; 返回值
;; ----
;; boolean
;; 写入成功返回 #t；日志关闭或写盘失败返回 #f。
;;
;; 注意
;; ----
;; 首行是元信息（http-error status=… provider=… limit=…），随后用
;; “----- body begin -----” 与 “----- body end -----” 包裹响应体原文，便于 grep。

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

;; 6. 额度耗尽类错误附带额度类型（按业务码判定，不看消息文本）
(pi-log-http-error! 429
  "{\"error\":{\"code\":\"1308\",\"message\":\"您已达到5小时用量上限\"}}"
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
