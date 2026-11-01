;; validate-check-config!
;; 严格校验检查配置对象的格式、类型与取值范围。
;;
;; 语法
;; ----
;; (validate-check-config! cfg)
;;
;; 参数
;; ----
;; cfg : json-object?
;; 待校验的配置 JSON 对象。
;;
;; 返回值
;; ----
;; json-object?
;; 校验通过时返回原配置对象。
;;
;; 错误处理
;; --------
;; 当配置非 JSON 对象、字段类型不符合要求时抛出 type-error。
;; 当规则名未知、阈值不合法时抛出 value-error。

(import (scheme base)
  (liii check)
  (liii json)
  (liii goldcheck config)
) ;import

(check-set-mode! 'report-failed)

;; 1. 合法配置对象校验通过并返回自身
(let* ((cfg (string->json "{\"goldfish_version\": \"18.11\", \"rules\": {\"max-file-lines\": {\"limit\": 400}}}"))
       (res (validate-check-config! cfg))
      ) ;let*
  (check-true (json-object? res))
) ;let*

;; 2. 非 JSON 对象输入抛出 type-error
(check-catch 'type-error (validate-check-config! 123))
(check-catch 'type-error (validate-check-config! "not-an-object"))

;; 3. goldfish_version 非字符串抛出 type-error
(check-catch 'type-error
  (validate-check-config! (string->json "{\"goldfish_version\": 18}"))
) ;check-catch

;; 4. rules 中 limit 非整数抛出 type-error
(check-catch 'type-error
  (validate-check-config! (string->json "{\"rules\": {\"max-file-lines\": {\"limit\": \"400\"}}}"))
) ;check-catch

;; 5. rules 中 limit 为 0 或负数抛出 value-error
(check-catch 'value-error
  (validate-check-config! (string->json "{\"rules\": {\"max-file-lines\": {\"limit\": 0}}}"))
) ;check-catch

(check-catch 'value-error
  (validate-check-config! (string->json "{\"rules\": {\"max-file-lines\": {\"limit\": -10}}}"))
) ;check-catch

;; 6. 未知规则名抛出 value-error
(check-catch 'value-error
  (validate-check-config! (string->json "{\"rules\": {\"nonexistent-rule\": {\"limit\": 10}}}"))
) ;check-catch

(check-report)
