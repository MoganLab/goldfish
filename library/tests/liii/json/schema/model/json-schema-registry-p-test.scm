;; json-schema-registry?
;; 判断对象是否为资源登记表记录。
;;
;; 语法
;; ----
;; (json-schema-registry? value)
;;
;; 参数
;; ----
;; value : any?
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是资源登记表记录返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 登记表记录由 make-registry 创建，
;; 也可能由 (liii json schema registry) 的登记函数返回。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema model))

(check-set-mode! 'report-failed)

(check-true (json-schema-registry? (make-registry '())))
(check-true
  (json-schema-registry?
    (make-registry '(("urn:integer" ("type" . "integer"))))
  ) ;json-schema-registry?
) ;check-true

;; 其它对象不是登记表记录。

(check-false (json-schema-registry? '()))
(check-false (json-schema-registry? #t))
(check-false (json-schema-registry? "registry"))

(check-report)
