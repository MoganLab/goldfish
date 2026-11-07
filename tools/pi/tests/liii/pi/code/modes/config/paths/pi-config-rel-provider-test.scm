;; pi-config-rel-provider
;; 生成 provider 配置文件相对配置目录的路径。
;;
;; 语法
;; ----
;; (pi-config-rel-provider p-id)
;;
;; 参数
;; ----
;; p-id : string
;; provider 标识。
;;
;; 返回值
;; ----
;; string
;; 相对配置目录的配置文件路径。

(import (liii check) (liii pi code modes config paths))

(check-set-mode! 'report-failed)

;; 1. 相对路径格式为 models/<provider>.json
(check (pi-config-rel-provider "google") => "models/google.json")
(check (pi-config-rel-provider "kimi-coding") => "models/kimi-coding.json")

(check-report)
