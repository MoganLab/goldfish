;; %make-pi-skill
;; 构造 <pi-skill> 技能记录（name、description、file-path、base-dir 与
;; disable-model-invocation? 五字段）。
;;
;; 语法
;; ----
;; (%make-pi-skill name description file-path base-dir disable?) -> pi-skill?
;;
;; 参数
;; ----
;; name : string
;; 技能名称。
;; description : string
;; 技能描述。
;; file-path : string
;; SKILL.md 的绝对路径。
;; base-dir : string
;; 技能根目录（SKILL.md 父目录）。
;; disable? : boolean
;; 是否禁止模型自动调用。
;;
;; 返回值
;; ------
;; pi-skill?
;; 技能记录。

(import (scheme base)
  (liii check)
  (liii pi code core skills types)
) ;import

(check-set-mode! 'report-failed)

(let ((skill (%make-pi-skill "pdf-tools"
               "Extract text from PDF files"
               "/path/pdf/SKILL.md"
               "/path/pdf"
               #f
             ) ;%make-pi-skill
       ) ;skill
      )
  (check-true (pi-skill? skill))
  (check (pi-skill-name skill) => "pdf-tools")
) ;let

(check-report)
