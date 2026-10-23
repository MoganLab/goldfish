(import (scheme base) (liii check) (liii json) (liii goldcheck config))

;; check-rule-limit
;; 查询指定规则在配置中的数值阈值上限。未配置时回退默认内置上限。
;;
;; 语法
;; ----
;; (check-rule-limit cfg rule-name)
;;
;; 参数
;; ----
;; cfg : json-object 或 #f
;; 检查配置对象。
;; rule-name : string
;; 规则名称，如 "max-file-lines"、"max-tree-depth"。
;;
;; 返回值
;; ------
;; integer
;; 规则的上限值（默认 max-file-lines 为 300，max-tree-depth 为 15）。

(check-set-mode! 'report-failed)

;; 1. cfg 为 #f 时返回默认内置阈值
(check (check-rule-limit #f "max-file-lines") => 400)
(check (check-rule-limit #f "max-tree-depth") => 15)

;; 2. 读取配置中的自定义阈值
(let ((cfg (string->json "{\"rules\": {\"max-file-lines\": {\"limit\": 500}, \"max-tree-depth\": {\"limit\": 20}}}"
           ) ;string->json
      ) ;cfg
     ) ;
  (check (check-rule-limit cfg "max-file-lines") => 500)
  (check (check-rule-limit cfg "max-tree-depth") => 20)
) ;let

;; 3. 配置中未写 limit 字段时回退默认值
(let ((cfg (string->json "{\"rules\": {\"max-file-lines\": {\"enabled\": true}}}")))
  (check (check-rule-limit cfg "max-file-lines") => 400)
  (check (check-rule-limit cfg "max-tree-depth") => 15)
) ;let

(check-report)
