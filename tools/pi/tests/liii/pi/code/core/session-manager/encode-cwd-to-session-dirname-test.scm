;; encode-cwd-to-session-dirname
;; 将工作目录路径编码为会话目录名。
;;
;; 语法
;; ----
;; (encode-cwd-to-session-dirname cwd)
;;
;; 参数
;; ----
;; cwd : string
;; 工作目录路径。
;;
;; 返回值
;; ----
;; string
;; 编码后的会话目录名。

(import (liii check) (liii pi code core session-manager))

(check-set-mode! 'report-failed)

(check (encode-cwd-to-session-dirname "/home/da/git/goldfish")
  =>
  "--home-da-git-goldfish--"
) ;check
(check (encode-cwd-to-session-dirname "/var/log") => "--var-log--")

;; Windows: 反斜杠与正斜杠必须编码为同一扁平目录名（对齐 pi 的
;; --${cwd.replace(/^[/\\]/, "").replace(/[/\\:]/g, "-")}-- ）
(check (encode-cwd-to-session-dirname "C:\\Users\\darcy\\git\\goldfish2")
  =>
  "--C--Users-darcy-git-goldfish2--"
) ;check
(check (encode-cwd-to-session-dirname "C:/Users/darcy/git/goldfish2")
  =>
  "--C--Users-darcy-git-goldfish2--"
) ;check
;; 盘符相对根路径（前导 \ 剥一位）
(check (encode-cwd-to-session-dirname "\\Users\\darcy") => "--Users-darcy--")

(check-report)
