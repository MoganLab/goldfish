;; restrict-check-config
;; 改写配置使得仅指定的规则处于 enabled 状态，保持原有 excludes 配置。
;;
;; 语法
;; ----
;; (restrict-check-config cfg only-name)
;;
;; 参数
;; ----
;; cfg : list          原始检查配置（JSON alist 结构）
;; only-name : string  仅启用的规则名
;;
;; 返回值
;; ------
;; list
;; 修改后的配置，仅目标规则 enabled 为 #t，其余规则 enabled 为 #f。

(import (scheme base) (liii check) (liii json) (liii goldcheck only))

(check-set-mode! 'report-failed)

(let* ((cfg
         '(("rules"
            ("max-file-lines" ("enabled" . #t) ("limit" . 400))
            ("no-crlf" ("enabled" . #t)))
           ("excludes" ("no-glue" "bench/")))
       ) ;cfg
       (res (restrict-check-config cfg "no-crlf"))
       (rules (json-ref res "rules"))
       (mfl (json-ref rules "max-file-lines"))
       (nc (json-ref rules "no-crlf"))
      ) ;
  (check (json-ref nc "enabled") => #t)
  (check (json-ref mfl "enabled") => #f)
  (check (json-ref mfl "limit") => 400)
  (check (json-ref res "excludes") => '(("no-glue" "bench/")))
) ;let*

(check-report)
