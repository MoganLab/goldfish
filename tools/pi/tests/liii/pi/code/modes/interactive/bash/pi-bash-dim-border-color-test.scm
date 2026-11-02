;; pi-bash-dim-border-color
;; Bash 排除模式（!! 命令）下暗色边框的 ANSI 颜色常量。
;;
;; 语法
;; ----
;; pi-bash-dim-border-color
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string?
;; ANSI 颜色转义序列。

(import (scheme base) (liii check) (liii pi code modes interactive bash))

(check-set-mode! 'report-failed)

(check-true (string? pi-bash-dim-border-color))
(check-true (> (string-length pi-bash-dim-border-color) 0))

(check-report)
