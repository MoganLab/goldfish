;; make-pi-config
;; 构造带默认值与关键字参数的 <pi-config> 记录。
;;
;; 语法
;; ----
;; (make-pi-config [keyword val ...]) -> <pi-config>
;;
;; 参数
;; ----
;; keyword val : any（可选）
;; 运行时配置字段的关键字与取值，未传入的字段取默认值。
;;
;; 返回值
;; ------
;; <pi-config>
;; 初始化后的运行时配置记录。

(import (liii check) (liii pi code config query))

(check-set-mode! 'report-failed)

(let ((cfg (make-pi-config :provider "deepseek" :model-id "deepseek-v4-pro")))
  (check-true (pi-config? cfg))
  (check (pi-cfg-ref cfg 'provider) => "deepseek")
  (check (pi-cfg-ref cfg 'model-id) => "deepseek-v4-pro")
  (check (pi-cfg-ref cfg 'thinking-level) => "medium")
  (check (pi-cfg-ref cfg 'tools) => '())
) ;let

(check-report)
