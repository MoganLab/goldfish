;; make-skill-card
;; 构造技能折叠卡片组件（对标上游 SkillInvocationMessageComponent）：
;; 折叠时单行渲染「[skill] name (Ctrl+O 展开)」，展开时显示标题与正文。
;;
;; 语法
;; ----
;; (make-skill-card name content :expanded? bool) -> component?
;;
;; 参数
;; ----
;; name : string
;; 技能名称。
;; content : string
;; 技能正文 Markdown。
;; expanded? : boolean
;; 初始展开状态（默认 #f）。
;;
;; 返回值
;; ------
;; component?
;; 技能卡片组件。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive views skill-card)
) ;import

(check-set-mode! 'report-failed)

(let* ((card (make-skill-card "pdf-tools"
               "References are relative to /path.\n\n# PDF Guide\nDetails."
             ) ;make-skill-card
       ) ;card
       (w 60)
      ) ;
  (check-true (component? card))

  ;; 折叠渲染：单行紧凑卡片 + 空行
  (let ((lines (component-render card w)))
    (check (length lines) => 2)
    (check-true (string-contains? (car lines) "[skill] pdf-tools (Ctrl+O 展开)"))
    (check (cadr lines) => "")
  ) ;let

  ;; 展开渲染：标题与正文
  (skill-card-set-expanded! card #t)
  (let ((lines (component-render card w)))
    (check-true (> (length lines) 2))
    (let ((full (string-join lines "\n")))
      (check-true (string-contains? full "[skill] **pdf-tools**"))
      (check-true (string-contains? full "# PDF Guide"))
    ) ;let
  ) ;let

  ;; 再次折叠
  (skill-card-set-expanded! card #f)
  (let ((lines (component-render card w)))
    (check (length lines) => 2)
  ) ;let
) ;let*

(check-report)
