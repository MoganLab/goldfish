;; pi-bash-schema
;; bash 工具的 JSON schema 定义
;;
;; 语法
;; ----
;; (pi-bash-schema)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list?，工具 schema

(import (liii check)
  (liii pi code core tools bash)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

;; pi-bash-schema
;; 获取 bash 工具的 JSON Schema 定义。
(let* ((schema (pi-bash-schema))
       (props (cdr (assoc "properties" schema)))
       (timeout-prop (cdr (assoc "timeout" props)))
       (desc (cdr (assoc "description" timeout-prop)))
      ) ;
  (check (cdr (assoc "type" schema)) => "object")
  (check-true (pair? props))
  (check desc => "Timeout in seconds (optional, defaults to 600s)")
) ;let*

;; pi-bash-schema 动态默认超时参数
(let* ((schema (pi-bash-schema 300))
       (props (cdr (assoc "properties" schema)))
       (timeout-prop (cdr (assoc "timeout" props)))
       (desc (cdr (assoc "description" timeout-prop)))
      ) ;
  (check desc => "Timeout in seconds (optional, defaults to 300s)")
) ;let*

(check-report)
