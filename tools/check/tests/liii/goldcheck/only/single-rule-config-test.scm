;; single-rule-config
;; 构造仅启用指定规则的新配置，保留该规则自身的配置项。
;;
;; 语法
;; ----
;; (single-rule-config cfg rule-name all-rule-names)
;;
;; 参数
;; ----
;; cfg : list             原始检查配置
;; rule-name : string     需要启用的单一规则名称
;; all-rule-names : list  全部规则名称列表
;;
;; 返回值
;; ------
;; list
;; 新的配置，其中仅 rule-name 规则的 enabled 设为 #t，其他全部规则 enabled 设为 #f。

(import (scheme base) (liii check) (liii json) (liii goldcheck only))

(check-set-mode! 'report-failed)

(let* ((cfg
         '(("rules" ("max-file-lines" ("limit" . 300)) ("no-crlf"))
           ("version" . "1.0"))
       ) ;cfg
       (all '("max-file-lines" "no-crlf" "no-glue"))
       (res (single-rule-config cfg "max-file-lines" all))
       (rules (json-ref res "rules"))
       (mfl (json-ref rules "max-file-lines"))
       (nc (json-ref rules "no-crlf"))
       (ng (json-ref rules "no-glue"))
      ) ;
  (check (json-ref mfl "enabled") => #t)
  (check (json-ref mfl "limit") => 300)
  (check (json-ref nc "enabled") => #f)
  (check (json-ref ng "enabled") => #f)
  (check (json-ref res "version") => "1.0")
) ;let*

;; cfg 为 #f 时的边界情况
(let* ((all '("rule-a" "rule-b"))
       (res (single-rule-config #f "rule-a" all))
       (rules (json-ref res "rules"))
      ) ;
  (check (json-ref (json-ref rules "rule-a") "enabled") => #t)
  (check (json-ref (json-ref rules "rule-b") "enabled") => #f)
) ;let*

(check-report)
