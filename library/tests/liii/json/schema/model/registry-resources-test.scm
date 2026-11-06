;; registry-resources
;; 读取资源登记表记录中的资源列表。
;;
;; 语法
;; ----
;; (registry-resources registry)
;;
;; 参数
;; ----
;; registry : registry
;; 资源登记表记录。
;;
;; 返回值
;; ----
;; list
;; 资源列表，每一项是「URI . Schema」的 alist 单元。
;;
;; 注意
;; ----
;; 返回的是记录内部保存的列表，
;; 登记新资源会生成新的记录，不会修改这份列表。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema model))

(check-set-mode! 'report-failed)

;; 空登记表没有资源。

(check (registry-resources (make-registry '())) => '())

;; 带资源的登记表按登记顺序保存资源。

(define registry
  (make-registry '(("urn:integer" ("type" . "integer"))
                   ("urn:string" ("type" . "string")))
  ) ;make-registry
) ;define

(check (length (registry-resources registry)) => 2)
(check (assoc "urn:integer" (registry-resources registry))
  =>
  '("urn:integer" ("type" . "integer"))
) ;check
(check (assoc "urn:string" (registry-resources registry))
  =>
  '("urn:string" ("type" . "string"))
) ;check

(check-report)
