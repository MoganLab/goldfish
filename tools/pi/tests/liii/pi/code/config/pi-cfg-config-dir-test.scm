;; pi-cfg-config-dir
;; 从运行配置中取 JSON 配置文件目录，缺失时回退 agent 目录。
;;
;; 语法
;; ----
;; (pi-cfg-config-dir cfg)
;;
;; 参数
;; ----
;; cfg : alist
;; resolve-pi-config 生成的运行配置。
;;
;; 返回值
;; ----
;; any
;; config-dir 或 agent-dir 的值；两者都不存在时返回 #f。

(import (liii check) (liii pi code config))

(check-set-mode! 'report-failed)

;; config-dir 优先
(check (pi-cfg-config-dir '((config-dir . "/tmp/gf-config")
                            (agent-dir . "/tmp/pi-agent"))
       ) ;pi-cfg-config-dir
  =>
  "/tmp/gf-config"
) ;check

;; 缺少 config-dir 时回退 agent-dir
(check (pi-cfg-config-dir '((agent-dir . "/tmp/pi-agent"))) => "/tmp/pi-agent")

;; 空配置或非 alist 返回 #f
(check (pi-cfg-config-dir '()) => #f)
(check (pi-cfg-config-dir #f) => #f)

(check-report)
