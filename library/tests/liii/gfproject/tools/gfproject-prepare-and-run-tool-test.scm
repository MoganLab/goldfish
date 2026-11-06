;; gfproject-prepare-and-run-tool
;; 准备执行环境并运行指定工具。
;;
;; 语法
;; ----
;; (gfproject-prepare-and-run-tool command tool-config gf-lib allow-fallback) -> integer or #f
;;
;; 参数
;; ----
;; command : string
;; 工具命令名称。
;;
;; tool-config : alist
;; 工具配置字典。
;;
;; gf-lib : string
;; goldfish 库根路径。
;;
;; allow-fallback : boolean
;; 失败时是否允许回退到内置命令。
;;
;; 返回值
;; ----
;; integer or #f
;; 工具进程退出码，或者在允许回退且执行失败时返回 #f。

(import (liii check)
  (liii gfproject config)
  (liii gfproject tools)
) ;import

(check-set-mode! 'report-failed)

;; 1. 指定不存在的 tools_dir 时严格返回 1（禁用 fallback）
(let* ((gf-lib (gfproject-get-gf-lib))
       (tool-cfg-no-fallback '(("tools_dir" . "~/non_existent_tools_dir_xyz_12345")
                               ("organization" . "liii")
                               ("module" . "goldversion"))
       ) ;tool-cfg-no-fallback
       (err-buf (open-output-string))
       (old-err (set-current-error-port err-buf))
       (res (gfproject-prepare-and-run-tool "version" tool-cfg-no-fallback gf-lib #t))
      ) ;
  (set-current-error-port old-err)
  (check res => 1)
) ;let*

;; 2. 未指定 tools_dir 且模块不存在时，allow-fallback 为 #t 返回 #f（允许 fallback）
(let* ((gf-lib (gfproject-get-gf-lib))
       (tool-cfg-broken '(("organization" . "liii")
                          ("module" . "non_existent_module_xyz_12345"))
       ) ;tool-cfg-broken
       (res (gfproject-prepare-and-run-tool "test-tool" tool-cfg-broken gf-lib #t))
      ) ;
  (check res => #f)
) ;let*

(check-report)
