;; pi-skill-file-path
;; 读取技能 SKILL.md 的绝对路径。
;;
;; 语法
;; ----
;; (pi-skill-file-path skill) -> string
;;
;; 参数
;; ----
;; skill : pi-skill?
;; 技能记录。
;;
;; 返回值
;; ------
;; string
;; SKILL.md 绝对路径。

(import (scheme base) (liii check) (liii pi code core skills types))

(check-set-mode! 'report-failed)

(let ((skill (%make-pi-skill "pdf-tools" "desc" "/path/pdf/SKILL.md" "/path/pdf" #f))
     ) ;
  (check (pi-skill-file-path skill) => "/path/pdf/SKILL.md")
) ;let

(check-report)
