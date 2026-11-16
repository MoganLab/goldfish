;; pi-retry-config->json
;; 将 <pi-retry-config> record 转换为 Scheme-JSON 对象（alist）。
;;
;; 语法
;; ----
;; (pi-retry-config->json r) -> alist
;;
;; 参数
;; ----
;; r : pi-retry-config? | alist
;; 重试配置 record 或已是 JSON alist。
;;
;; 返回值
;; ----
;; alist
;; 可由 json->string 序列化的 JSON 对象，且可被 json->pi-retry-config 往返还原。

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi ai config retry)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? pi-retry-config->json))

;; 基本转换与往返还原
(let* ((orig (pi-retry-config #f 5 1500 30000 2))
       (j (pi-retry-config->json orig))
       (rt (json->pi-retry-config (string->json (json->string j))))
      ) ;
  (check (cdr (assoc "enabled" j)) => 'false)
  (check (cdr (assoc "maxRetries" j)) => 5)
  (check (cdr (assoc "baseDelayMs" j)) => 1500)
  (check (cdr (assoc "maxAgentDelayMs" j)) => 30000)
  (check (cdr (assoc "maxRetries" (cdr (assoc "provider" j)))) => 2)
  (check-true (pi-retry-config? rt))
  (check (pi-retry-config-enabled rt) => #f)
  (check (pi-retry-config-max-retries rt) => 5)
  (check (pi-retry-config-base-delay-ms rt) => 1500)
  (check (pi-retry-config-max-agent-delay-ms rt) => 30000)
  (check (pi-retry-config-provider-max-retries rt) => 2)
) ;let*

;; provider-max-retries 为 #f 时省略 provider 字段
(let* ((orig (pi-retry-config #t 3 2000 60000 #f))
       (j (pi-retry-config->json orig))
       (rt (json->pi-retry-config (string->json (json->string j))))
      ) ;
  (check (cdr (assoc "enabled" j)) => 'true)
  (check (assoc "provider" j) => #f)
  (check (pi-retry-config-provider-max-retries rt) => #f)
) ;let*

;; 幂等性：传入 alist 原样返回
(let ((al '(("enabled" . true))))
  (check (pi-retry-config->json al) => al)
) ;let

(check-report)
