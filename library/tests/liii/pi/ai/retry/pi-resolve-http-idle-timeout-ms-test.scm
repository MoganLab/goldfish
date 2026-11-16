;; pi-resolve-http-idle-timeout-ms
;; 解析 settings 中的 httpIdleTimeoutMs，缺省 300000ms，0 或 "disabled" 返回 0。
;;
;; 语法
;; ----
;; (pi-resolve-http-idle-timeout-ms settings)
;;
;; 参数
;; ----
;; settings : pi-settings? | list?
;; 全局设置或 alist。
;;
;; 返回值
;; ------
;; number?
;; 空闲超时毫秒数，0 表示关闭。

(import (liii check) (liii pi ai config record) (liii pi ai retry))

(check-set-mode! 'report-failed)

(check (pi-resolve-http-idle-timeout-ms '()) => 300000)
(check (pi-resolve-http-idle-timeout-ms (pi-settings :http-idle-timeout-ms 45000)) => 45000)
(check (pi-resolve-http-idle-timeout-ms (pi-settings :http-idle-timeout-ms 0)) => 0)
(check (pi-resolve-http-idle-timeout-ms (pi-settings :http-idle-timeout-ms "disabled")) => 0)

(check-report)
