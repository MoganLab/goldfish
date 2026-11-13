;; gemini-log-finish-reason-error!
;; 将 HTTP 200 但 finishReason 映射为 error 的请求与响应体原文落盘到 pi 日志。
;;
;; 语法
;; ----
;; (gemini-log-finish-reason-error! finish-reason body provider model-id url attempt max-attempts)
;;
;; 参数
;; ----
;; finish-reason : string | #f
;; 响应返回的原始 finishReason（如 SAFETY、MALFORMED_RESPONSE）。
;; body : string
;; 响应体原文（JSON 或 SSE 文本）。
;; provider : string
;; 服务商 id。
;; model-id : string | #f
;; 模型 id。
;; url : string
;; 请求 URL。
;; attempt : integer
;; 当前尝试次数。
;; max-attempts : integer
;; 最大尝试次数。
;;
;; 返回值
;; ----
;; boolean
;; 写入成功返回 #t；日志关闭或写盘失败返回 #f。

(import (liii check)
  (liii string)
  (liii path)
  (liii tempfile)
  (liii pi ai log)
  (liii pi ai api google-generative-ai stream-result)
) ;import

(check-set-mode! 'report-failed)

(define *base* (make-temp-dir "gf-pi-log-fr-" ""))
(define *dir* (path-join (path *base*) "logs"))

(pi-log-reset!)
(pi-log-init! *dir*)

(gemini-log-finish-reason-error! "MALFORMED_RESPONSE"
  "{\"candidates\":[{\"finishReason\":\"MALFORMED_RESPONSE\"}]}"
  "google"
  "gemini-4-hydrogen"
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-4-hydrogen:streamGenerateContent?alt=sse"
  1
  3
) ;gemini-log-finish-reason-error!

(let ((content (path-read-text (pi-log-file-path))))
  (check-true (string-contains? content "status=200"))
  (check-true (string-contains? content "reason=MALFORMED_RESPONSE"))
  (check-true (string-contains? content "retryable=yes"))
  (check-true (string-contains? content "model=gemini-4-hydrogen"))
) ;let

(pi-log-reset!)
(path-unlink (pi-log-file-path))
(path-rmdir *dir*)
(path-rmdir *base*)

(check-report)
