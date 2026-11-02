;; pi-app-config-command?
;; 判定输入文本是否为 /config 斜杠命令。
;;
;; 语法
;; ----
;; (pi-app-config-command? text)
;;
;; 参数
;; ----
;; text : string?
;; 用户输入的命令文本。
;;
;; 返回值
;; ----
;; boolean?
;; 当且仅当文本精确匹配 "/config" 时返回 #t，否则返回 #f。

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
