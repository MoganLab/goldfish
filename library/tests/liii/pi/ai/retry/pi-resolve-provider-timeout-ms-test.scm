;; pi-resolve-provider-timeout-ms
;; 解析非流式请求总超时（毫秒）。
;;
;; 语法
;; ----
;; (pi-resolve-provider-timeout-ms settings [opt-timeout-ms])
;;
;; 参数
;; ----
;; settings : pi-settings? | list?
;; 全局设置或 alist。
;; opt-timeout-ms : number? | #f，可选
;; 显式覆盖的超时毫秒数。
;;
;; 返回值
;; ------
;; number? | #f
;; 生效的毫秒数，关闭时为 #f。

(import (liii check) (liii pi ai config record) (liii pi ai retry))

(check-set-mode! 'report-failed)

(check (pi-resolve-provider-timeout-ms '()) => 300000)
(check (pi-resolve-provider-timeout-ms (pi-settings :http-idle-timeout-ms 120000)) => 120000)
(check (pi-resolve-provider-timeout-ms
         (pi-settings :http-idle-timeout-ms 120000
           :retry (pi-retry-config :provider-timeout-ms 90000)
         ) ;pi-settings
       ) ;pi-resolve-provider-timeout-ms
  =>
  90000
) ;check
(check (pi-resolve-provider-timeout-ms
         (pi-settings :http-idle-timeout-ms 120000
           :retry (pi-retry-config :provider-timeout-ms 90000)
         ) ;pi-settings
         15000
       ) ;pi-resolve-provider-timeout-ms
  =>
  15000
) ;check
(check (pi-resolve-provider-timeout-ms (pi-settings :http-idle-timeout-ms "disabled")) => #f)

(check-report)
