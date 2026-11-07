;; pi-app-skills
;; 读取交互式应用实例记录的已加载技能列表。
;;
;; 语法
;; ----
;; (pi-app-skills app) -> list
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; list
;; 已加载的 <pi-skill> 列表（默认空列表）。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code core skills types)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (pi-app-skills app) => '())
) ;let

(let* ((skill (%make-pi-skill "pdf-tools" "desc" "/p/SKILL.md" "/p" #f))
       (app (make-pi-app
              :terminal (make-virtual-terminal :width 80 :height 24)
              :skills   (list skill)
            ) ;make-pi-app
       ) ;app
      ) ;
  (check (length (pi-app-skills app)) => 1)
  (check (pi-skill-name (car (pi-app-skills app))) => "pdf-tools")
) ;let*

(check-report)
