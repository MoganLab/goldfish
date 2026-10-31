;; 公共 API 的错误访问器与注册表断言暂存于此，
;; 后续分别拆分到 tests/liii/json/schema/error/ 与
;; tests/liii/json/schema/registry/ 下的按函数测试文件。
(import (liii json schema) (liii check))

(check-set-mode! 'report-failed)

(define schema
  '(("type" . "object")
    ("properties" ("name" ("type" . "string")))
    ("required" . #("name")))
) ;define

;; Structured errors and immutable registries are available from the same entry.
(let ((error (car (json-schema-errors '(("name" . 1)) schema))))
  (check-true (json-schema-error? error))
  (check (json-schema-error-kind error) => 'validation)
  (check (json-schema-error-keyword error) => "type")
  (check (json-schema-error-path error) => '("name"))
  (check (json-schema-error-absolute-path error) => '("name"))
  (check (json-schema-error-json-path error) => "$.name")
) ;let

(define empty (make-json-schema-registry))

(define filled
  (json-schema-registry-add-many empty '(("urn:string" ("type" . "string"))))
) ;define
(check-true (json-schema-registry? filled))
(check-true
  (json-schema-valid? "x" '(("$ref" . "urn:string")) `((registry . ,filled)))
) ;check-true
(check-catch 'json-schema-options-error
  (json-schema-registry-add filled "urn:string" #t)
) ;check-catch

(check-report)
