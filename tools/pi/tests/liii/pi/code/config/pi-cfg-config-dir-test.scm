;; pi-cfg-config-dir
;; 从解析后的 cfg 中取 JSON 配置文件目录：优先 config-dir，缺失时回退 agent-dir。
;;
;; 语法
;; ----
;; (pi-cfg-config-dir cfg) -> any
;;
;; 参数
;; ----
;; cfg : alist
;; resolve-pi-config 返回的配置。
;;
;; 返回值
;; ----
;; any
;; 配置目录；cfg 为空或缺少目录项时返回 #f。

(import (liii check)
  (liii pi code config)
) ;import

(check-set-mode! 'report-failed)

(check (pi-cfg-config-dir '((config-dir . "/tmp/cfg") (agent-dir . "/tmp/agent")))
  =>
  "/tmp/cfg"
) ;check
(check (pi-cfg-config-dir '((agent-dir . "/tmp/agent"))) => "/tmp/agent")
(check (pi-cfg-config-dir '()) => #f)
(check (pi-cfg-config-dir #f) => #f)

(check-report)
