(import (liii check)
  (liii string)
  (liii pi ai providers quota))

(check-set-mode! 'report-failed)

;; pi-quota-error-message
;; 从错误响应体安全提取 error.message。
;;
;; 语法
;; ----
;; (pi-quota-error-message body-str)
;;
;; 返回值
;; ----
;; 正常返回 message 字段值（可能为 '() 等非字符串，由调用方过滤）；
;; 解析失败（非 JSON、畸形结构）返回 '()，并经注入的 logger 记录。
;;
;; 注意
;; ----
;; logger 由 (liii pi ai log) 初始化时注入（quota 不能反向 import log，
;; 循环依赖），未注入时静默。本文件用手动注入验证。

;; 1. 正常提取（嵌套 error.message，pretty JSON）
(check (pi-quota-error-message
         "{\n  \"error\": {\n    \"message\": \"quota exceeded\"\n  }\n}")
  => "quota exceeded"
) ;check

;; 2. 无 error 字段 -> '()（json-ref 安全路径，不触发 logger）
(check (pi-quota-error-message "{}") => '())

;; 3. error 为字符串 -> '()（json-ref 安全路径）
(check (pi-quota-error-message "{\"error\":\"plain\"}") => '())

;; 4. 空 body -> '()
(check (pi-quota-error-message "") => '())

;; 5. 非 JSON：返回 '() 且触发注入的 logger
(let ((logged '()))
  (set-pi-quota-error-logger! (lambda (msg) (set! logged (cons msg logged))))
  (check (pi-quota-error-message "<html>429</html>") => '())
  (check-true (and (pair? logged)
                (string-contains? (car logged) "quota-parse-error")
                (string-contains? (car logged) "<html>429</html>")
          ) ;and
  ) ;check-true
  (set-pi-quota-error-logger! #f)
) ;let

;; 6. 畸形结构（error 是数组）：返回 '() 且触发 logger
(let ((logged '()))
  (set-pi-quota-error-logger! (lambda (msg) (set! logged (cons msg logged))))
  (check (pi-quota-error-message "{\"error\":[\"a\"]}") => '())
  (check-true (pair? logged))
  (set-pi-quota-error-logger! #f)
) ;let

;; 7. 超长 body 截断为 120 字符 + "..."
(let ((logged '()))
  (set-pi-quota-error-logger! (lambda (msg) (set! logged (cons msg logged))))
  (pi-quota-error-message (string-append "<" (make-string 300 #\a)))
  (check-true
    (and (pair? logged)
      (string-contains? (car logged) "...")
      (<= (string-length (car logged))
        (+ (string-length "quota-parse-error tag=parse-error body=") 123)
      ) ;<=
    ) ;and
  ) ;check-true
  (set-pi-quota-error-logger! #f)
) ;let

(check-report)
