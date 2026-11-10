;; pi-skill-description
;; 读取技能描述。
;;
;; 语法
;; ----
;; (pi-skill-description skill) -> string
;;
;; 参数
;; ----
;; skill : pi-skill?
;; 技能记录。
;;
;; 返回值
;; ------
;; string
;; 技能描述。

(import (scheme base) (liii check) (liii pi code core skills types))

(check-set-mode! 'report-failed)

(let ((skill (%make-pi-skill "pdf-tools" "Extract text" "/p/SKILL.md" "/p" #f)))
  (check (pi-skill-description skill) => "Extract text")
) ;let

(check-report)
