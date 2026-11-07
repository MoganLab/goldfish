;; pi-config-rel-model
;; 生成模型配置文件相对配置目录的路径。
;;
;; 语法
;; ----
;; (pi-config-rel-model p-id m-id)
;;
;; 参数
;; ----
;; p-id : string
;; provider 标识。
;;
;; m-id : string
;; 模型标识。
;;
;; 返回值
;; ----
;; string
;; 相对配置目录的模型配置文件路径。

(import (liii check)
  (liii pi code modes config paths)
) ;import

(check-set-mode! 'report-failed)

;; 1. 相对路径格式为 models/<provider>/<model>.json
(check (pi-config-rel-model "kimi-coding" "k3.1-pro")
  => "models/kimi-coding/k3.1-pro.json"
) ;check

(check-report)
