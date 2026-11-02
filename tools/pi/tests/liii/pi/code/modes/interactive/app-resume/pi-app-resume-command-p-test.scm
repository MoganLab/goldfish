;; pi-app-resume-command?
;; 判定输入文本是否为 /resume 会话恢复斜杠命令。
;;
;; 语法
;; ----
;; (pi-app-resume-command? text)
;;
;; 参数
;; ----
;; text : string
;; 待检验的输入文本字符串。
;;
;; 返回值
;; ------
;; boolean
;; 为精确匹配 /resume 或以 /resume 空格开头时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii pi code modes interactive app))

(check-set-mode! 'report-failed)

;; 1. 正例匹配
(check-true (pi-app-resume-command? "/resume"))
(check-true (pi-app-resume-command? "/resume path/to/file"))

;; 2. 负例排除
(check-false (pi-app-resume-command? "resume"))
(check-false (pi-app-resume-command? "/resumex"))
(check-false (pi-app-resume-command? "/help"))

(check-report)
