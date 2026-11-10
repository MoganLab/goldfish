;; pi-skill-disable-model-invocation?
;; 读取技能是否禁止模型自动调用（frontmatter 的 disable-model-invocation）。
;;
;; 语法
;; ----
;; (pi-skill-disable-model-invocation? skill) -> boolean
;;
;; 参数
;; ----
;; skill : pi-skill?
;; 技能记录。
;;
;; 返回值
;; ------
;; boolean
;; 禁止模型自动调用时为 #t。

(import (scheme base) (liii check) (liii pi code core skills types))

(check-set-mode! 'report-failed)

(check-false (pi-skill-disable-model-invocation? (%make-pi-skill "pdf-tools" "desc" "/p/SKILL.md" "/p" #f)
             ) ;pi-skill-disable-model-invocation?
) ;check-false

(check-true (pi-skill-disable-model-invocation? (%make-pi-skill "admin-task" "desc" "/a/SKILL.md" "/a" #t)
            ) ;pi-skill-disable-model-invocation?
) ;check-true

(check-report)
