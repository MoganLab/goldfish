;; pi-model-entry-api
;; 读取 <pi-model-entry> 的 api 字段。
;;
;; 语法
;; ----
;; (pi-model-entry-api entry) -> string | #f

;;
;; 参数
;; ----
;; entry : pi-model-entry?
;;
;; 返回值
;; ----
;; any

(import (scheme base) (liii check) (liii pi code config types))

(check-set-mode! 'report-failed)

(check (pi-model-entry-api (make-pi-model-entry #f #f #f "openai")) => "openai")
(check (pi-model-entry-api (make-pi-model-entry)) => #f)

(check-report)
