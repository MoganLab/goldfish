;; %repl-symbol-name?
;; 判断符号名是否已收录在符号缓存中。
;;
;; 语法
;; ----
;; (%repl-symbol-name? token)
;;
;; 参数
;; ----
;; token : string
;; 待查找的符号名。
;;
;; 返回值
;; ----
;; boolean
;; 符号名存在于缓存中时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldrepl complete))

(check-set-mode! 'report-failed)

(%repl-update-symbol-cache!)

;; 已收录的符号名
(check (%repl-symbol-name? "string-append") => #t)

;; 不存在的符号名
(check (%repl-symbol-name? "no-such-symbol-xyz-abc") => #f)

(check-report)
