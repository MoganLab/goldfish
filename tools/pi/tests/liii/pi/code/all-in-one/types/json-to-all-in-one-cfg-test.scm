;; json->all-in-one-cfg
;; 将 JSON 对象转换为 <all-in-one-cfg> record。
;;
;; 语法
;; ----
;; (json->all-in-one-cfg obj)
;; (json->all-in-one-cfg obj cfg-path)
;;
;; 参数
;; ----
;; obj : any?
;; string->json 解析出的 alist，或已是 <all-in-one-cfg> 的 record。
;; cfg-path : string? (可选)
;; 用于告警日志的配置文件路径。
;;
;; 返回值
;; ----
;; all-in-one-cfg? | #f
;; 解析成功返回 <all-in-one-cfg>，非对象或 chains 非数组时返回 #f。

(import (liii check) (liii json) (liii pi code all-in-one types))

(check-set-mode! 'report-failed)

(check-true (procedure? json->all-in-one-cfg))

;; 1. 完整配置解析（含秒级参数换算与链解析）
(let* ((j (string->json "{\"continuous\":false,\"maxWait\":120,\"retryInterval\":30,\"probeTimeout\":5,\"chains\":[{\"id\":\"fast\",\"nodes\":[{\"provider\":\"deepseek\",\"model\":\"deepseek-chat\"}]}]}"))
       (cfg (json->all-in-one-cfg j))
      ) ;
  (check-true (all-in-one-cfg? cfg))
  (check (all-in-one-cfg-continuous? cfg) => #f)
  (check (all-in-one-cfg-max-wait-ms cfg) => 120000)
  (check (all-in-one-cfg-retry-interval-ms cfg) => 30000)
  (check (all-in-one-cfg-probe-timeout-ms cfg) => 5000)
  (check (length (all-in-one-cfg-chains cfg)) => 1)
  (check (chain-def-id (car (all-in-one-cfg-chains cfg))) => "fast")
) ;let*

;; 2. 缺省字段使用默认值（含毫秒字段兼容）
(let ((cfg1 (json->all-in-one-cfg (string->json "{}")))
      (cfg2 (json->all-in-one-cfg (string->json "{\"maxWaitMs\":5000,\"retryIntervalMs\":2000,\"probeTimeoutMs\":1000}")))
     ) ;
  (check-true (all-in-one-cfg? cfg1))
  (check (all-in-one-cfg-continuous? cfg1) => #t)
  (check (all-in-one-cfg-max-wait-ms cfg1) => 3600000)
  (check (all-in-one-cfg-retry-interval-ms cfg1) => 60000)
  (check (all-in-one-cfg-probe-timeout-ms cfg1) => 8000)
  (check (all-in-one-cfg-chains cfg1) => '())
  (check (all-in-one-cfg-max-wait-ms cfg2) => 5000)
  (check (all-in-one-cfg-retry-interval-ms cfg2) => 2000)
  (check (all-in-one-cfg-probe-timeout-ms cfg2) => 1000)
) ;let

;; 3. 非法输入与 chains 非数组返回 #f
(check-false (json->all-in-one-cfg '()))
(check-false (json->all-in-one-cfg "bad"))
(check-false (json->all-in-one-cfg (string->json "{\"chains\":\"not-array\"}") "test.json"))

;; 4. 已是 record 原样返回
(let* ((orig (make-all-in-one-cfg #t 1000 500 200 '()))
       (out (json->all-in-one-cfg orig))
      ) ;
  (check (eq? out orig) => #t)
) ;let*

(check-report)
