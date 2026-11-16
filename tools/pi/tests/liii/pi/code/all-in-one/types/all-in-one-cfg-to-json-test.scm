;; all-in-one-cfg->json
;; 将 <all-in-one-cfg> record 转换为 Scheme-JSON 对象（alist）。
;;
;; 语法
;; ----
;; (all-in-one-cfg->json cfg) -> alist
;;
;; 参数
;; ----
;; cfg : all-in-one-cfg? | alist
;; all-in-one 全局配置 record 或已是 JSON alist。
;;
;; 返回值
;; ----
;; alist
;; 可由 json->string 序列化的 JSON 对象，且可被 json->all-in-one-cfg 往返还原。

(import (scheme base)
  (liii check)
  (liii json)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? all-in-one-cfg->json))

(let* ((orig (make-all-in-one-cfg #f
               120000
               30000
               5000
               (list (make-chain-def "fast"
                       "Fast"
                       (list (make-chain-node "deepseek" "deepseek-chat" "low"))
                     ) ;make-chain-def
               ) ;list
             ) ;make-all-in-one-cfg
       ) ;orig
       (j (all-in-one-cfg->json orig))
       (rt (json->all-in-one-cfg (string->json (json->string j))))
      ) ;
  (check (cdr (assoc "provider" j)) => "all-in-one")
  (check (cdr (assoc "continuous" j)) => 'false)
  (check (cdr (assoc "maxWaitMs" j)) => 120000)
  (check (cdr (assoc "retryIntervalMs" j)) => 30000)
  (check (cdr (assoc "probeTimeoutMs" j)) => 5000)
  (check-true (all-in-one-cfg? rt))
  (check (all-in-one-cfg-continuous? rt) => #f)
  (check (all-in-one-cfg-max-wait-ms rt) => 120000)
  (check (all-in-one-cfg-retry-interval-ms rt) => 30000)
  (check (all-in-one-cfg-probe-timeout-ms rt) => 5000)
  (check (length (all-in-one-cfg-chains rt)) => 1)
) ;let*

(check-catch 'type-error (all-in-one-cfg->json '(("provider" . "all-in-one"))))
(check-catch 'type-error (all-in-one-cfg->json "invalid"))

(check-report)
