;; gfproject-resolve-tool
;; 解析工具的配置信息。
;;
;; 语法
;; ----
;; (gfproject-resolve-tool command [opt-gf-lib]) -> alist or #f
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
;; 成功时返回工具的配置字典，未找到返回 #f。

(import (liii check)
  (liii json)
  (liii gfproject tools)
) ;import

(check-set-mode! 'report-failed)

(let ((tool-info (gfproject-resolve-tool "doc")))
  (check-true (pair? tool-info))
  (check (json-ref tool-info "module") => "golddoc")
) ;let

(check (gfproject-resolve-tool "non-existent-tool-xyz") => #f)

(check-report)
