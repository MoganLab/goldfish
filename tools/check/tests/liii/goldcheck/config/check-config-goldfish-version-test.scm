;; check-config-goldfish-version
;; 获取检查配置中声明的 goldfish_version 字符串。
;;
;; 语法
;; ----
;; (check-config-goldfish-version cfg)
;;
;; 参数
;; ----
;; cfg : json-object? 或 #f
;; 检查配置对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 若配置中包含有效的 goldfish_version 返回其字符串值，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii json)
  (liii goldcheck config)
) ;import

(check-set-mode! 'report-failed)

;; 1. cfg 为 #f 时返回 #f
(check-false (check-config-goldfish-version #f))

;; 2. 配置中包含 goldfish_version 时返回对应字符串
(let ((cfg (string->json "{\"goldfish_version\": \"18.11\"}")))
  (check (check-config-goldfish-version cfg) => "18.11")
) ;let

;; 3. 配置中未写 goldfish_version 时返回 #f
(let ((cfg (string->json "{\"rules\": {}}")))
  (check-false (check-config-goldfish-version cfg))
) ;let

(check-report)
