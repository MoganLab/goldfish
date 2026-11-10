;; pi-slash-command-matches
;; 斜杠菜单技能命令：enable-skill-commands? 开启时动态混入 /skill:<name>
;; 命令并参与模糊匹配，关闭时不出现。
;;
;; 语法
;; ----
;; (pi-slash-command-matches text :skills skills :enable-skill-commands? flag) -> list | #f
;;
;; 参数
;; ----
;; text : string
;; 编辑器当前输入。
;; skills : list
;; 已加载的 <pi-skill> 列表。
;; enable-skill-commands? : boolean
;; 是否混入 /skill: 命令。
;;
;; 返回值
;; ------
;; list | #f
;; 匹配命令列表；无匹配或非命令输入为 #f。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code core skills types)
  (liii pi code modes interactive slash-command-menu)
) ;import

(check-set-mode! 'report-failed)

(let* ((s1 (%make-pi-skill "pdf-tools" "PDF 提取" "/path/pdf/SKILL.md" "/path/pdf" #f)
       ) ;s1
       (s2 (%make-pi-skill "admin-task" "运维指令" "/path/admin/SKILL.md"
             "/path/admin" #t
           ) ;%make-pi-skill
       ) ;s2
       (skills (list s1 s2))
      ) ;

  ;; 1. 当开启 enable-skill-commands 时，输入 /skill: 匹配全部技能
  (let ((matches (pi-slash-command-matches "/skill:" skills #t)))
    (check-true (pair? matches))
    (check (length matches) => 2)
    (let ((names (map (lambda (m) (cdr (assoc 'name m))) matches)))
      (check-true (pair? (member "skill:pdf-tools" names)))
      (check-true (pair? (member "skill:admin-task" names)))
    ) ;let
  ) ;let

  ;; 2. 模糊匹配过滤
  (let ((matches (pi-slash-command-matches "/skill:pdf" skills #t)))
    (check-true (pair? matches))
    (check (length matches) => 1)
    (check (cdr (assoc 'name (car matches))) => "skill:pdf-tools")
    (check (cdr (assoc 'description (car matches))) => "PDF 提取")
  ) ;let

  ;; 3. 生成菜单组件展示
  (let* ((matches (pi-slash-command-matches "/skill:pdf" skills #t))
         (menu (make-pi-slash-command-menu matches))
        ) ;
    (check-true (component? menu))
    (let ((items (select-list-items menu)))
      (check (length items) => 1)
      (check (cdr (assoc 'value (car items))) => "skill:pdf-tools")
      (check (cdr (assoc 'label (car items))) => "/skill:pdf-tools")
      (check (cdr (assoc 'description (car items))) => "PDF 提取")
    ) ;let
  ) ;let*

  ;; 4. 当禁用 enable-skill-commands 时，输入 /skill: 不匹配任何技能
  (let ((matches (pi-slash-command-matches "/skill:" skills #f)))
    (check matches => #f)
  ) ;let
) ;let*

(check-report)
