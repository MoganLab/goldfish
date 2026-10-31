(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive slash-command-menu)
  (liii pi code modes interactive app-config)
) ;import

(check-set-mode! 'report-failed)

;; /config：在交互界面里打开三列配置 TUI

;; 1. 命令检测：精确匹配 "/config"
(check (pi-app-config-command? "/config") => #t)
(check (pi-app-config-command? "/config extra") => #f)
(check (pi-app-config-command? "/configx") => #f)
(check (pi-app-config-command? "config") => #f)

;; 2. 斜杠菜单能补全出 config
(let* ((matches (pi-slash-command-matches "/con"))
       (names (map (lambda (c) (cdr (assoc 'name c))) matches))
      ) ;
  (check (and (member "config" names) #t) => #t)
) ;let*

;; 3. 输入 "/config"（带参数）时不再弹菜单
(check (pi-slash-command-matches "/config ") => #f)

(check-report)
