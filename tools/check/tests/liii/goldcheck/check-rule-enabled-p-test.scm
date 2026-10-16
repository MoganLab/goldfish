(import (scheme base) (liii check) (liii json) (liii goldcheck-config))

;; check-rule-enabled?
;; 查询指定规则是否在配置中处于启用状态（enabled 为 #t 或未声明时默认为 #t）。
;;
;; 语法
;; ----
;; (check-rule-enabled? cfg rule-name)
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
;; boolean
;; #t 表示规则启用，#f 表示规则已禁用。

(check-set-mode! 'report-failed)

;; 1. cfg 为 #f 时默认启用
(check (check-rule-enabled? #f "max-file-lines") => #t)
(check (check-rule-enabled? #f "max-tree-depth") => #t)

;; 2. rules 中显式声明 enabled 为 #f
(let ((cfg (string->json "{\"rules\": {\"max-file-lines\": {\"enabled\": false, \"limit\": 300}}}"
           ) ;string->json
      ) ;cfg
     ) ;
  (check (check-rule-enabled? cfg "max-file-lines") => #f)
  ;; 未显式声明的规则保持默认 #t
  (check (check-rule-enabled? cfg "max-tree-depth") => #t)
) ;let

;; 3. 显式声明 enabled 为 #t
(let ((cfg (string->json "{\"rules\": {\"max-tree-depth\": {\"enabled\": true, \"limit\": 15}}}"
           ) ;string->json
      ) ;cfg
     ) ;
  (check (check-rule-enabled? cfg "max-tree-depth") => #t)
) ;let

(check-report)
