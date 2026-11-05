;; pi-ai-log!
;; 写入一条 AI 层通用日志。
;;
;; 语法
;; ----
;; (pi-ai-log! msg)
;; (pi-ai-log! msg port)
;;
;; 参数
;; ----
;; msg : string?
;; 日志消息文本。
;;
;; port : port?
;; 可选，未注入 logger 时的备选输出端口。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base) (liii check) (liii string) (liii pi ai retry))

(check-set-mode! 'report-failed)

;; 1. 通过端口捕获输出
(let ((p (open-output-string)))
  (pi-ai-log! "hello retry log" p)
  (check-true (string-contains? (get-output-string p) "hello retry log"))
) ;let

;; 2. 通过注入 logger 拦截
(let ((captured #f))
  (set-pi-retry-logger! (lambda (msg) (set! captured msg)))
  (pi-ai-log! "intercepted")
  (check captured => "intercepted")
  (set-pi-retry-logger! #f)
) ;let

(check-report)
