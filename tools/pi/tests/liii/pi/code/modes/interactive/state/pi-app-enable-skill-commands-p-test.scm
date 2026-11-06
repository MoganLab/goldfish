;; pi-app-enable-skill-commands?
;; 读取交互式应用实例的 /skill: 命令开关（来自 settings.json 的
;; enableSkillCommands，默认 #t）。
;;
;; 语法
;; ----
;; (pi-app-enable-skill-commands? app) -> boolean
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; boolean
;; 允许 /skill: 展示与展开时为 #t。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(check-true
  (pi-app-enable-skill-commands?
    (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))
  ) ;pi-app-enable-skill-commands?
) ;check-true

(check-false
  (pi-app-enable-skill-commands?
    (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24)
      :enable-skill-commands #f
    ) ;make-pi-app
  ) ;pi-app-enable-skill-commands?
) ;check-false

(check-report)
