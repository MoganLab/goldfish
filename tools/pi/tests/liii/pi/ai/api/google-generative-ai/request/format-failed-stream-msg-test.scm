;; format-failed-stream-msg
;; 根据失败响应生成流式请求的错误提示信息。
;;
;; 语法
;; ----
;; (format-failed-stream-msg status resp)
;;
;; 参数
;; ----
;; status : integer
;; HTTP 状态码；0 表示网络错误或连接失败。
;; resp : alist | procedure | hash-table?
;; 响应对象，可通过 reason 或 error 字段提供错误描述。
;;
;; 返回值
;; ----
;; string
;; 错误提示信息。

(import (liii check)
  (liii pi ai api google-generative-ai request)
) ;import

(check-set-mode! 'report-failed)

;; 带 reason 的响应
(check (format-failed-stream-msg 0 '((reason . "connection refused")))
  => "Request failed: connection refused")

;; 带 error 字段的响应
(check (format-failed-stream-msg 500 '((error . "server error")))
  => "Request failed: server error")

;; reason 优先于 error
(check (format-failed-stream-msg 0 '((reason . "first") (error . "second")))
  => "Request failed: first")

;; 空白 reason 被裁剪后视为空
(check (format-failed-stream-msg 0 '((reason . "   "))) => "HTTP 0 stream error")

;; 没有错误描述时使用状态码兜底
(check (format-failed-stream-msg 500 '()) => "HTTP 500 stream error")
(check (format-failed-stream-msg 404 '()) => "HTTP 404 stream error")

(check-report)
