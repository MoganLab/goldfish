;; pi-resolve-timeout
;; 解析请求超时秒数。
;;
;; 语法
;; ----
;; (pi-resolve-timeout parsed cfg)
;;
;; 参数
;; ----
;; parsed : alist
;; 解析后的参数字典。
;;
;; cfg : alist
;; Agent 配置 alist。
;;
;; 返回值
;; ----
;; integer
;; 生效的超时秒数，缺省为 600。

(import (liii check) (liii pi ai config record) (liii pi code cli args query))

(check-set-mode! 'report-failed)

;; 1. 显式参数优先（含 0 与 disabled 关闭语义）
(check (pi-resolve-timeout '((timeout . 30)) '()) => 30)
(check (pi-resolve-timeout '((timeout . 0)) '()) => 0)
(check (pi-resolve-timeout '((timeout . "disabled")) '()) => 0)

;; 2. 回退到配置值或 settings.httpIdleTimeoutMs（毫秒转秒）
(check (pi-resolve-timeout '() '((timeout . 120))) => 120)
(check
  (pi-resolve-timeout '()
    (list (cons 'settings (pi-settings :http-idle-timeout-ms 45000)))
  ) ;pi-resolve-timeout
  =>
  45.0
) ;check
(check
  (pi-resolve-timeout '()
    (list (cons 'settings (pi-settings :http-idle-timeout-ms "disabled")))
  ) ;pi-resolve-timeout
  =>
  0.0
) ;check

;; 3. 默认 300 秒（对应 httpIdleTimeoutMs 默认 300000ms）
(check (pi-resolve-timeout '() '()) => 300)

;; 4. 非法取值回退默认
(check (pi-resolve-timeout '((timeout . "x")) '()) => 300)
(check (pi-resolve-timeout '((timeout . -1)) '()) => 300)

(check-report)
