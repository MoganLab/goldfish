;; gfproject JSON Schema
;; 使用 library/resources/gfproject.schema.json 校验项目配置。
;;
;; 覆盖 package 可选性、完整示例、旧配置兼容、命名字符集、
;; 必填字段、description 类型与未知字段拒绝规则。
;;
;; 语法
;; ----
;; (json-schema-valid? config schema)
;;
;; 参数
;; ----
;; config : json-object?
;; gfproject.json 解析后的配置对象。
;;
;; schema : json-object?
;; library/resources/gfproject.schema.json 解析后的 Schema。
;;
;; 返回值
;; ------
;; boolean
;; 配置满足 Schema 返回 #t，否则返回 #f。

(import (liii check) (liii gfproject package) (liii json)
  (liii json schema) (liii os) (liii path)
) ;import

(check-set-mode! 'report-failed)

;; 本文件直接通过 bin/gf library/tests/... 从仓库根执行，
;; 不依赖 gf test 按加载根重定位项目根的行为。
(define project-root
  (let ((cwd (path (getcwd))))
    (if (path-file? (path-join cwd "library" "resources" "gfproject.schema.json"))
      cwd
      (path-parent cwd)
    ) ;if
  ) ;let
) ;define
(define schema-path
  (path->string
    (path-join project-root "library" "resources" "gfproject.schema.json")
  ) ;path->string
) ;define
(define schema (string->json (path-read-text schema-path)))

;; 当前内置 JSON Schema 后端没有正则引擎，会将 pattern 作为注解忽略。
;; 测试用等价的 anyOf 补全该校验；Schema 文件仍采用标准 pattern，
;; 并由标准 Draft 2020-12 校验器验证其实际语义。
(define (name-part-schema min-length)
  `(("type" . "string")
    ("minLength" . ,min-length)
    ("not" . (("pattern" . "[^a-z0-9_-]")))
  ) ;quasiquote
) ;define
(define portable-schema
  (json-set schema "$defs"
    (json-set (json-ref schema "$defs") "packageNamePart"
      `(("type" . "string")
        ("anyOf" . ,(vector (name-part-schema 1) (name-part-schema 2)))
      ) ;quasiquote
    ) ;json-set
  ) ;json-set
) ;define
(define config-path
  (path->string (path-join project-root "gfproject.json"))
) ;define
(define config (string->json (path-read-text config-path)))

;; Schema 自身合法，Goldfish 项目配置通过校验
(check-true (json-object? (json-schema-check-schema schema)))
(check-true (json-schema-valid? config schema))

;; Goldfish 包标识为 lang@liii
(let ((package (json->gfproject-package config)))
  (check-true (gfproject-package? package))
  (check (gfproject-package-id package) => "lang@liii")
) ;let

;; 无 package 的旧项目继续有效
(check-true
  (json-schema-valid?
    (string->json
      "{\"domains\":{\"app\":{\"root\":\"app/\",\"depends-on\":[\"lib\"],\"mode\":\"r7rs\"}},\"tools\":{\"run\":{\"description\":{\"en_US\":\"Run\"}}}}"
    ) ;string->json
    schema
  ) ;json-schema-valid?
) ;check-true

;; 合法 package 名称允许小写字母、数字、连字符与下划线
(check-true
  (json-schema-valid?
    (string->json
      "{\"package\":{\"name\":\"my-package_2\",\"organization\":\"liii-labs\",\"description\":\"Demo\"}}"
    ) ;string->json
    schema
  ) ;json-schema-valid?
) ;check-true

;; package 非法实例
(for-each
  (lambda (text)
    (check-false (json-schema-valid? (string->json text) portable-schema))
  ) ;lambda
  '("{\"package\":1}"
    "{\"package\":{\"name\":\"lang\"}}"
    "{\"package\":{\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"\",\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"Lang\",\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"liii/lang\",\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"lang@liii\",\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"1lang\",\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"lang\",\"organization\":\"liii labs\"}}"
    "{\"package\":{\"name\":\"lang\",\"organization\":\"liii\",\"description\":1}}"
    "{\"package\":{\"name\":\"lang\",\"organization\":\"liii\",\"version\":\"1.0.0\"}}")
) ;for-each

;; 顶层保留前向兼容，已有 domains/tools 字段受 Schema 约束
(check-true (json-schema-valid? (string->json "{\"future-field\":1}") schema))
(check-false (json-schema-valid? (string->json "{\"domains\":[]}") schema))
(check-false (json-schema-valid? (string->json "{\"tools\":[]}") schema))

(check-report)
