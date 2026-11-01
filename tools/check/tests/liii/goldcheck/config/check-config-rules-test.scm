;; check-config-rules
;; 获取检查配置中声明的 rules 对象。
;;
;; 语法
;; ----
;; (check-config-rules cfg)
;;
;; 参数
;; ----
;; cfg : json-object? 或 #f
;; 检查配置对象。
;;
;; 返回值
;; ----
;; json-object? 或 list?
;; 配置中的 rules JSON 对象；若 cfg 为 #f 或未配置 rules 返回空列表。

(import (scheme base)
  (liii check)
  (liii json)
  (liii goldcheck config)
) ;import

(check-set-mode! 'report-failed)

;; 1. cfg 为 #f 时返回空列表
(check (check-config-rules #f) => '(()))

;; 2. 读取配置中的 rules 字段
(let* ((cfg (string->json "{\"rules\": {\"max-file-lines\": {\"limit\": 400}}}"))
       (rules (check-config-rules cfg))
      ) ;let*
  (check-true (json-object? rules))
  (check-true (json-contains-key? rules "max-file-lines"))
) ;let*

(check-report)
