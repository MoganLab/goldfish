;; json->gfproject-package
;; 从 gfproject 配置 JSON 对象解析可选的 package 元数据。
;;
;; 语法
;; ----
;; (json->gfproject-package config)
;;
;; 参数
;; ----
;; config : json-object?
;; gfproject.json 解析后的 JSON 对象。
;;
;; 返回值
;; ----
;; gfproject-package? or #f
;; 有 package 字段时返回包记录；没有 package 字段时返回 #f。
;;
;; 错误处理
;; ----
;; 配置或 package 不是 JSON 对象时抛出 type-error。
;; 缺少 name/organization、字段非法或含未知字段时抛出 value-error。

(import (liii check) (liii gfproject package) (liii json))

(check-set-mode! 'report-failed)

;; 完整 package 元数据
(let ((package
        (json->gfproject-package
          (string->json
            "{\"package\":{\"name\":\"lang\",\"organization\":\"liii\",\"description\":\"Goldfish Scheme\"}}"
          ) ;string->json
        ) ;json->gfproject-package
      ) ;package
     ) ;
  (check-true (gfproject-package? package))
  (check (gfproject-package-name package) => "lang")
  (check (gfproject-package-organization package) => "liii")
  (check (gfproject-package-description package) => "Goldfish Scheme")
  (check (gfproject-package-id package) => "lang@liii")
) ;let

;; description 可选，缺省为空字符串
(let ((package
        (json->gfproject-package
          (string->json "{\"package\":{\"name\":\"pi\",\"organization\":\"liii\"}}")
        ) ;json->gfproject-package
      ) ;package
     ) ;
  (check (gfproject-package-name package) => "pi")
  (check (gfproject-package-organization package) => "liii")
  (check (gfproject-package-description package) => "")
  (check (gfproject-package-version package) => #f)
  (check (gfproject-package-id package) => "pi@liii")
) ;let

;; version 可选字符串，缺省为 #f
(let ((package
        (json->gfproject-package
          (string->json
            "{\"package\":{\"name\":\"pi\",\"organization\":\"liii\",\"version\":\"2026.11.24\"}}"
          ) ;string->json
        ) ;json->gfproject-package
      ) ;package
     ) ;
  (check (gfproject-package-version package) => "2026.11.24")
) ;let

;; package 整体可选，保持旧项目兼容
(check-false (json->gfproject-package (string->json "{\"domains\":{}}")))

;; 非法配置与 package 类型
(check-catch 'type-error (json->gfproject-package '()))
(check-catch 'type-error (json->gfproject-package (string->json "{\"package\":1}")))
(check-catch 'type-error (json->gfproject-package (string->json "{\"package\":[]}")))

;; 必填字段、命名字符集与 description 类型
(for-each
  (lambda (text)
    (check-catch 'value-error (json->gfproject-package (string->json text)))
  ) ;lambda
  '("{\"package\":{\"name\":\"lang\"}}"
    "{\"package\":{\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"\",\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"Lang\",\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"liii/lang\",\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"lang@liii\",\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"1lang\",\"organization\":\"liii\"}}"
    "{\"package\":{\"name\":\"lang\",\"organization\":\"liii labs\"}}"
    "{\"package\":{\"name\":\"lang\",\"organization\":\"liii\",\"description\":1}}"
    "{\"package\":{\"name\":\"lang\",\"organization\":\"liii\",\"version\":1}}")
) ;for-each

;; version 现为已知字段，解析器接受字符串值
(let ((package
        (json->gfproject-package
          (string->json
            "{\"package\":{\"name\":\"lang\",\"organization\":\"liii\",\"version\":\"1.0.0\"}}"
          ) ;string->json
        ) ;json->gfproject-package
      ) ;package
     ) ;
  (check (gfproject-package-version package) => "1.0.0")
) ;let

;; ID 访问器强校验记录类型
(check-catch 'type-error (gfproject-package-id '(("name" . "lang"))))

(check-report)
