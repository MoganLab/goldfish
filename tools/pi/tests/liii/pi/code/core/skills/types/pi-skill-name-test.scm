;; pi-skill-name
;; 读取技能名称。
;;
;; 语法
;; ----
;; (pi-skill-name skill) -> string
;;
;; 参数
;; ----
;; skill : pi-skill?
;; 技能记录。
;;
;; 返回值
;; ------
;; string
;; 技能名称。

(import (scheme base)
  (liii check)
  (liii pi code core skills types)
) ;import

(check-set-mode! 'report-failed)

(let ((skill (%make-pi-skill "pdf-tools" "desc" "/p/SKILL.md" "/p" #f)))
  (check (pi-skill-name skill) => "pdf-tools"))

(check-report)
