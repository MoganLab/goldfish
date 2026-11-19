;; openai-build-headers
;; 构造发往 OpenAI Chat Completions API 的 HTTP 请求头。
;;
;; 语法
;; ----
;; (openai-build-headers api-key)
;;
;; 参数
;; ----
;; api-key : string? | #f
;; API 访问密钥。
;;
;; 返回值
;; ----
;; alist
;; 包含 Content-Type、Authorization 与 User-Agent 的请求头列表。

(import (liii check)
  (liii pi ai api openai-completions request)
) ;import

(check-set-mode! 'report-failed)

;; 正常密钥
(let ((headers (openai-build-headers "sk-test-123")))
  (check (cdr (assoc "Content-Type" headers)) => "application/json")
  (check (cdr (assoc "Authorization" headers)) => "Bearer sk-test-123")
  (check (cdr (assoc "User-Agent" headers)) => "Goldfish-Pi/1.0")
) ;let

;; 空密钥或 #f
(let ((headers (openai-build-headers #f)))
  (check (cdr (assoc "Authorization" headers)) => "Bearer ")
) ;let

;; 携带附加请求头
(let ((headers (openai-build-headers "sk-test" (list (cons "x-opencode-session" "sess-42")))))
  (check (cdr (assoc "x-opencode-session" headers)) => "sess-42")
) ;let

;; ${API_KEY} 每次构造请求头时展开，密钥轮换不会沿用旧 key。
(let ((headers (openai-build-headers "key-one"
                 '(("x-goog-api-key" . "${API_KEY}") ("x-pair" . "${API_KEY}-suffix"))
               ) ;openai-build-headers
      ) ;headers
     ) ;
  (check (cdr (assoc "x-goog-api-key" headers)) => "key-one")
  (check (cdr (assoc "x-pair" headers)) => "key-one-suffix")
) ;let
(let ((headers (openai-build-headers "key-two" '(("x-goog-api-key" . "${API_KEY}")))))
  (check (cdr (assoc "x-goog-api-key" headers)) => "key-two")
) ;let

;; 自定义 header 覆盖默认 header 时大小写不敏感，不产生重复 Authorization。
(let ((headers (openai-build-headers "default-key"
                 '(("authorization" . "Custom token") ("content-type" . "application/custom"))
               ) ;openai-build-headers
      ) ;headers
     ) ;
  (check (assoc "Authorization" headers) => #f)
  (check (assoc "Content-Type" headers) => #f)
  (check (cdr (assoc "authorization" headers)) => "Custom token")
  (check (cdr (assoc "content-type" headers)) => "application/custom")
) ;let

;; 非字符串请求头值直接报类型错误。
(check-catch 'type-error
  (openai-build-headers "key" '(("x-invalid" . 42)))
) ;check-catch

(check-report)
