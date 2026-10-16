(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii json)
  (liii goldcheck-config)
) ;import

;; load-check-config
;; 读取并解析 gf_check.json 配置文件，并严格校验字段类型与取值范围。
;;
;; 语法
;; ----
;; (load-check-config [path-or-dir])
;;
;; 参数
;; ----
;; path-or-dir : string（可选）
;; 配置文件路径或其所在目录。缺省时自动使用工程根目录下的 gf_check.json。
;;
;; 返回值
;; ------
;; json-object 或 #f
;; 校验通过时返回解析后的 JSON 配置对象；文件不存在时返回 #f。
;;
;; 错误
;; ----
;; type-error: 当字段类型不符合规范时抛出（如 limit 非整数、exclude 非数组）。
;; value-error: 当字段取值不合法时抛出（如 limit <= 0、未知规则名）。

(check-set-mode! 'report-failed)

;; 1. 不存在的文件返回 #f
(check (load-check-config "/nonexistent/dir/gf_check.json") => #f)

;; 2. 读取当前工程根目录真实 gf_check.json
(let ((cfg (load-check-config)))
  (check-true (not (not cfg)))
  (check (check-config-goldfish-version cfg) => "18.11")
  (check (check-rule-enabled? cfg "max-file-lines") => #t)
  (check (check-rule-limit cfg "max-file-lines") => 300)
  (check (check-rule-enabled? cfg "max-tree-depth") => #t)
  (check (check-rule-limit cfg "max-tree-depth") => 15)
) ;let

;; 3. 字段类型错误校验：limit 非整数抛出 type-error
(check-catch 'type-error
  (validate-check-config! (string->json "{\"rules\": {\"max-file-lines\": {\"limit\": \"not-an-integer\"}}}"
                          ) ;string->json
  ) ;validate-check-config!
) ;check-catch

;; 4. 字段类型错误校验：exclude 非数组抛出 type-error
(check-catch 'type-error
  (validate-check-config! (string->json "{\"exclude\": \"not-an-array\"}"))
) ;check-catch

;; 5. 字段取值错误校验：limit 为 0 或负数抛出 value-error
(check-catch 'value-error
  (validate-check-config! (string->json "{\"rules\": {\"max-file-lines\": {\"limit\": 0}}}")
  ) ;validate-check-config!
) ;check-catch

(check-catch 'value-error
  (validate-check-config! (string->json "{\"rules\": {\"max-file-lines\": {\"limit\": -10}}}")
  ) ;validate-check-config!
) ;check-catch

;; 6. 字段取值错误校验：未知规则名抛出 value-error
(check-catch 'value-error
  (validate-check-config! (string->json "{\"rules\": {\"unknown-rule-xyz\": {\"limit\": 10}}}")
  ) ;validate-check-config!
) ;check-catch

(check-report)
