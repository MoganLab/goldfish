;; format-skills-for-prompt
;; 生成注入系统提示词的 <skills> XML 结构：过滤 disable-model-invocation
;; 技能；无可见技能时返回空字符串；对 name/description/location 执行
;; XML 实体转义。
;;
;; 语法
;; ----
;; (format-skills-for-prompt skills) -> string
;;
;; 参数
;; ----
;; skills : list
;; <pi-skill> 列表。
;;
;; 返回值
;; ------
;; string
;; XML 区块字符串；无可见技能时为空字符串。

(import (scheme base)
  (liii check)
  (liii string)
  (liii pi code core skills types)
  (liii pi code core skills format)
) ;import

(check-set-mode! 'report-failed)

(define pdf
  (%make-pi-skill "pdf-tools" "提取 PDF 文本与图表 & <元数据>"
    "/path/pdf-tools/SKILL.md" "/path/pdf-tools" #f
  ) ;%make-pi-skill
) ;define

(define admin
  (%make-pi-skill "admin-task" "仅供内部显式调用" "/a/SKILL.md" "/a" #t)
) ;define

;; 空列表返回空字符串
(check (format-skills-for-prompt '()) => "")

;; 全部为私有技能时返回空字符串
(check (format-skills-for-prompt (list admin)) => "")

;; XML 结构、read 工具指引与相对路径说明
(let ((xml (format-skills-for-prompt (list pdf admin))))
  (check-true (string-contains? xml "<skills>"))
  (check-true (string-contains? xml "<available_skills>"))
  (check-true (string-contains? xml "<name>pdf-tools</name>"))
  (check-true (string-contains? xml "<location>/path/pdf-tools/SKILL.md</location>")
  ) ;check-true
  (check-true (string-contains? xml "Use the read tool"))
  ;; disable-model-invocation 的 admin-task 不出现在提示词中
  (check-false (string-contains? xml "admin-task"))
  ;; XML 实体转义：& 与 <元数据>
  (check-true (string-contains? xml "&amp;"))
  (check-true (string-contains? xml "&lt;元数据&gt;"))
  (check-false (string-contains? xml "& <元数据>"))
) ;let

(check-report)
