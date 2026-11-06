;; json-schema-registry-add-many
;; 批量向资源登记表中登记资源。
;;
;; 语法
;; ----
;; (json-schema-registry-add-many registry resources)
;;
;; 参数
;; ----
;; registry : registry
;; 资源登记表。
;;
;; resources : list
;; 资源列表，每一项是「URI . Schema」的 alist 单元。
;;
;; 返回值
;; ----
;; registry
;; 登记完所有资源后的新登记表。
;;
;; 注意
;; ----
;; 逐项调用 json-schema-registry-add，
;; 因此同样要求 URI 不重复。
;;
;; 错误处理
;; ----
;; 任何一项登记失败时抛出对应错误，
;; 已登记的部分不会回滚。

(import (liii check) (liii json schema registry) (liii json schema model))

(check-set-mode! 'report-failed)

(define registry (make-json-schema-registry))

;; 批量登记。

(define next-registry
  (json-schema-registry-add-many registry
    '(("urn:integer" ("type" . "integer"))
      ("urn:string" ("type" . "string"))
      ("urn:false" . #f))
  ) ;json-schema-registry-add-many
) ;define

(check (length (registry-resources next-registry)) => 3)
(check (assoc "urn:integer" (registry-resources next-registry))
  =>
  '("urn:integer" ("type" . "integer"))
) ;check
(check (assoc "urn:string" (registry-resources next-registry))
  =>
  '("urn:string" ("type" . "string"))
) ;check
(check (assoc "urn:false" (registry-resources next-registry))
  =>
  '("urn:false" . #f)
) ;check

;; 原登记表保持不变。

(check (registry-resources registry) => '())

;; 空列表返回原登记表。

(check (eq? (json-schema-registry-add-many registry '()) registry) => #t)

;; 重复登记会报错。

(check-catch 'json-schema-options-error
  (json-schema-registry-add-many registry
    '(("urn:integer" ("type" . "integer")) ("urn:integer" ("type" . "string")))
  ) ;json-schema-registry-add-many
) ;check-catch

(check-report)
