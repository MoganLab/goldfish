;; pi-skill-base-dir
;; 读取技能根目录（SKILL.md 父目录，相对路径解析基准）。
;;
;; 语法
;; ----
;; (pi-skill-base-dir skill) -> string
;;
;; 参数
;; ----
;; skill : pi-skill?
;; 技能记录。
;;
;; 返回值
;; ------
;; string
;; 技能根目录绝对路径。

(import (scheme base)
  (liii check)
  (liii pi code core skills types)
) ;import

(check-set-mode! 'report-failed)

(let ((skill (%make-pi-skill "pdf-tools" "desc" "/path/pdf/SKILL.md" "/path/pdf" #f)))
  (check (pi-skill-base-dir skill) => "/path/pdf"))

(check-report)
