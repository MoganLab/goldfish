;; pi-build-system-prompt
;; 组装系统提示词：传入技能列表时注入 <skills> 区块，override 时静默。
;;
;; 语法
;; ----
;; (pi-build-system-prompt tools cwd project-context ... :skills skills) -> string
;;
;; 参数
;; ----
;; tools : list
;; 生效工具列表。
;; cwd : string
;; 工作目录。
;; project-context : string | #f
;; 项目上下文。
;; skills : list
;; 已加载的 <pi-skill> 列表。
;;
;; 返回值
;; ------
;; string
;; 组装后的系统提示词。

(import (scheme base)
  (liii check)
  (liii string)
  (liii pi code core skills types)
  (liii pi code core prompt)
) ;import

(check-set-mode! 'report-failed)

;; 1. 传入技能列表时渲染包含 <skills> 区块
(let* ((s1 (%make-pi-skill "pdf-tools" "PDF 提取" "/path/pdf/SKILL.md" "/path/pdf" #f)
       ) ;s1
       (s2 (%make-pi-skill "secret" "私有" "/path/secret/SKILL.md" "/path/secret" #t)
       ) ;s2
       (sp (pi-build-system-prompt :skills (list s1 s2)))
      ) ;
  (check-true (string-contains? sp "<skills>"))
  (check-true (string-contains? sp "<available_skills>"))
  (check-true (string-contains? sp "<name>pdf-tools</name>"))
  ;; 隐藏技能被排除
  (check-false (string-contains? sp "secret"))
  ;; 验证 <skills> 在 <rules> 之后
  (let ((rules-pos (string-position "</rules>" sp))
        (skills-pos (string-position "<skills>" sp))
       ) ;
    (check-true (and rules-pos skills-pos (> skills-pos rules-pos)))
  ) ;let
) ;let*

;; 2. system-prompt-override 时静默排除 <skills>
(let* ((s1 (%make-pi-skill "pdf-tools" "PDF" "/path/SKILL.md" "/path" #f))
       (sp (pi-build-system-prompt
             :system-prompt-override "Custom system prompt"
             :skills                 (list s1)
           ) ;pi-build-system-prompt
       ) ;sp
      ) ;
  (check-false (string-contains? sp "<skills>"))
  (check-true (string-contains? sp "Custom system prompt"))
) ;let*

;; 3. 技能列表为空或全隐藏时，不注入 <skills> 节点
(let* ((sp (pi-build-system-prompt :skills '())))
  (check-false (string-contains? sp "<skills>"))
) ;let*

(let* ((s-hidden (%make-pi-skill "secret" "私有" "/path/SKILL.md" "/path" #t))
       (sp (pi-build-system-prompt :skills (list s-hidden)))
      ) ;
  (check-false (string-contains? sp "<skills>"))
) ;let*

(check-report)
