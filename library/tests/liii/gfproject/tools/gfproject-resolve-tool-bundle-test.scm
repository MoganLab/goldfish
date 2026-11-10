;; gfproject-resolve-tool-bundle
;; 解析包含本地与内置源的完整工具配置包。
;;
;; 语法
;; ----
;; (gfproject-resolve-tool-bundle command [opt-gf-lib]) -> alist or #f
;;
;; 参数
;; ----
;; command : string
;; 目标工具命令名称。
;;
;; opt-gf-lib : string (可选)
;; goldfish 库路径。
;;
;; 返回值
;; ----
;; alist or #f
;; 成功时返回包含 merged-tool、lib-tool、has-local-override 等字段的 bundle 字典，未找到返回 #f。

(import (liii check)
  (liii json)
  (liii gfproject tools)
) ;import

(check-set-mode! 'report-failed)

(let ((bundle (gfproject-resolve-tool-bundle "doc")))
  (check (not (not bundle)) => #t)
  (check (json-ref (cdr (assoc "merged-tool" bundle)) "module") => "golddoc")
) ;let

(check (gfproject-resolve-tool-bundle "non-existent-tool-xyz") => #f)

(check-report)
