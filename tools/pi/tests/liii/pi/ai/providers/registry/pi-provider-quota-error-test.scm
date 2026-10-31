(import (liii check)
  (liii pi ai providers quota)
  (liii pi ai providers registry)
) ;import

(check-set-mode! 'report-failed)

;; pi-provider-quota-error
;; 调用某个 provider 注册的额度耗尽识别函数。
;;
;; 语法
;; ----
;; (pi-provider-quota-error status-code body-str provider)
;;
;; 参数
;; ----
;; status-code : integer?
;; HTTP 状态码。
;;
;; body-str : string?
;; 响应体原文。
;;
;; provider : string? 或 symbol?
;; 服务商 id；为 #f 或未注册时不做判定。
;;
;; 返回值
;; ----
;; pi-quota-limit? 或 #f
;; 命中时返回判定结果 record，否则 #f。
;;
;; 注意
;; ----
;; 处理函数抛异常时本函数返回 #f：额度识别失败不应打断请求主流程。
;; zai 是 zai-coding-cn 的路由别名，两者行为一致。

(define weekly-body
  "{\"error\":{\"code\":\"1310\",\"message\":\"每周额度已用尽\"}}"
) ;define

;; 1. zai 别名与 zai-coding-cn 行为一致
(check (pi-provider-quota-error 429 weekly-body "zai")
  =>
  (pi-provider-quota-error 429 weekly-body "zai-coding-cn")
) ;check

;; 2. 命中时返回 <pi-quota-limit>
(check-true (pi-quota-limit? (pi-provider-quota-error 429 weekly-body "zai-coding-cn"))
) ;check-true

;; 3. 未注册 provider 不做判定
(check-false (pi-provider-quota-error 429 weekly-body "google"))

;; 4. provider 为 #f 时不做判定
(check-false (pi-provider-quota-error 429 weekly-body))

;; 5. 空响应体不做判定
(check-false (pi-provider-quota-error 429 "" "zai-coding-cn"))

(check-report)
