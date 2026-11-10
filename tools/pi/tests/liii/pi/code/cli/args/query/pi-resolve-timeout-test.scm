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

(import (liii check) (liii pi code cli args query))

(check-set-mode! 'report-failed)

;; 1. 显式参数优先
(check (pi-resolve-timeout '((timeout . 30)) '()) => 30)

;; 2. 回退到配置值
(check (pi-resolve-timeout '() '((timeout . 120))) => 120)

;; 3. 默认 600 秒
(check (pi-resolve-timeout '() '()) => 600)

;; 4. 非法取值回退默认
(check (pi-resolve-timeout '((timeout . "x")) '()) => 600)
(check (pi-resolve-timeout '((timeout . -1)) '()) => 600)

(check-report)
