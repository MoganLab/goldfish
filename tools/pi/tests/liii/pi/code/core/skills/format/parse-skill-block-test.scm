;; parse-skill-block
;; 解析用户消息中的技能展开块，逆向切分出 name、content 与 user-message
;; 三键；非技能消息返回 #f。
;;
;; 语法
;; ----
;; (parse-skill-block text) -> list | #f
;;
;; 参数
;; ----
;; text : string
;; 消息文本。
;;
;; 返回值
;; ------
;; list | #f
;; ((name . name) (content . content) (user-message . msg))；非技能消息为 #f。

(import (scheme base)
  (liii check)
  (liii pi code core skills format)
) ;import

(check-set-mode! 'report-failed)

;; 带参数：切分正文与尾部用户消息
(let* ((sample "<skill name=\"pdf-tools\" location=\"/path/SKILL.md\">
References are relative to /path.

# Guide
Do things.
</skill>

extract file.pdf")
       (parsed (parse-skill-block sample)))
  (check-true (pair? parsed))
  (check (cdr (assoc 'name parsed)) => "pdf-tools")
  (check (cdr (assoc 'content parsed))
    => "References are relative to /path.\n\n# Guide\nDo things."
  ) ;check
  (check (cdr (assoc 'user-message parsed)) => "extract file.pdf"))

;; 无用户消息的技能块
(let* ((sample "<skill name=\"solo\" location=\"/path/SKILL.md\">
Body only
</skill>")
       (parsed (parse-skill-block sample)))
  (check-true (pair? parsed))
  (check (cdr (assoc 'name parsed)) => "solo")
  (check (cdr (assoc 'user-message parsed)) => #f))

;; 非技能消息返回 #f
(check-false (parse-skill-block "regular message"))

(check-report)
