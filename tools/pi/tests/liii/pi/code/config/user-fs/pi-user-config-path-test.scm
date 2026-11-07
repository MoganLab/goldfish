;; pi-user-config-path
;; 拼接配置目录下的文件路径。
;;
;; 语法
;; ----
;; (pi-user-config-path config-dir . segments)
;;
;; 参数
;; ----
;; config-dir : (or string path)
;; 配置目录。
;;
;; segments : list
;; 路径段列表。
;;
;; 返回值
;; ----
;; string
;; 拼接后的文件路径。

(import (liii check)
  (liii pi code config user-fs)
) ;import

(check-set-mode! 'report-failed)

;; 1. 单级路径段
(check (pi-user-config-path "/tmp/agent" "settings.json")
  => "/tmp/agent/settings.json"
) ;check

;; 2. 多级路径段
(check (pi-user-config-path "/tmp/agent" "models" "kimi-coding.json")
  => "/tmp/agent/models/kimi-coding.json"
) ;check

(check-report)
