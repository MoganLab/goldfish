;; json->pi-retry-config
;; 将 JSON 对象（settings.json 中的 retry 字段）转换为 <pi-retry-config> record。
;;
;; 语法
;; ----
;; (json->pi-retry-config obj)
;;
;; 参数
;; ----
;; obj : any?
;; string->json 解析出的 alist，或已是 <pi-retry-config> 的 record。
;;
;; 返回值
;; ----
;; pi-retry-config?
;; 解析后的重试配置 record；缺失字段使用安全默认值。

(import (liii check) (liii json) (liii pi ai config record))

(check-set-mode! 'report-failed)

(check-true (procedure? json->pi-retry-config))

;; 1. 完整 JSON 解析（含 provider.maxRetries 与 JSON 布尔 true/false）
(let* ((j (string->json "{\"enabled\":false,\"maxRetries\":5,\"baseDelayMs\":1500,\"maxAgentDelayMs\":30000,\"provider\":{\"maxRetries\":2}}"))
       (r (json->pi-retry-config j))
      ) ;
  (check-true (pi-retry-config? r))
  (check (pi-retry-config-enabled r) => #f)
  (check (pi-retry-config-max-retries r) => 5)
  (check (pi-retry-config-base-delay-ms r) => 1500)
  (check (pi-retry-config-max-agent-delay-ms r) => 30000)
  (check (pi-retry-config-provider-max-retries r) => 2)
) ;let*

;; 2. 空对象与非对象输入：使用默认值
(let ((r1 (json->pi-retry-config (string->json "{}")))
      (r2 (json->pi-retry-config '()))
     ) ;
  (check-true (pi-retry-config? r1))
  (check (pi-retry-config-enabled r1) => #t)
  (check (pi-retry-config-max-retries r1) => 3)
  (check (pi-retry-config-base-delay-ms r1) => 2000)
  (check (pi-retry-config-max-agent-delay-ms r1) => 60000)
  (check (pi-retry-config-provider-max-retries r1) => #f)
  (check-true (pi-retry-config? r2))
  (check (pi-retry-config-enabled r2) => #t)
) ;let

;; 3. provider.maxRetries 为 null 时归一为 #f
(let ((r (json->pi-retry-config (string->json "{\"enabled\":true,\"provider\":{\"maxRetries\":null}}"))))
  (check (pi-retry-config-enabled r) => #t)
  (check (pi-retry-config-provider-max-retries r) => #f)
) ;let

;; 4. 已是 record 原样返回
(let* ((orig (pi-retry-config #f 7 1000 20000 1))
       (out (json->pi-retry-config orig))
      ) ;
  (check (eq? out orig) => #t)
) ;let*

(check-report)
