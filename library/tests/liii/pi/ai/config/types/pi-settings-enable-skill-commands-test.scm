;; pi-settings-enable-skill-commands
;; 读取 <pi-settings> 记录的 Agent Skills 命令开关（settings.json 的
;; enableSkillCommands，未配置时默认 #t）。
;;
;; 语法
;; ----
;; (pi-settings-enable-skill-commands settings) -> boolean
;;
;; 参数
;; ----
;; settings : pi-settings?
;; 全局设置记录。
;;
;; 返回值
;; ------
;; boolean
;; 允许 /skill: 展示与展开时为 #t。

(import (scheme base)
  (liii check)
  (liii pi ai config record)
) ;import

(check-set-mode! 'report-failed)

;; 默认 pi-settings 构造：未指定时为 #t
(check-true (pi-settings-enable-skill-commands (pi-settings)))

;; 显式构造
(check-false (pi-settings-enable-skill-commands (pi-settings :enable-skill-commands #f)))
(check-true (pi-settings-enable-skill-commands (pi-settings :enable-skill-commands #t)))

(check-report)
