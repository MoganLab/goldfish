(import (liii check)
  (liii pi code core session-manager)
) ;import

(check-set-mode! 'report-failed)

(check (encode-cwd-to-session-dirname "/home/da/git/goldfish")
  => "--home-da-git-goldfish--")
(check (encode-cwd-to-session-dirname "/var/log") => "--var-log--")

;; Windows: 反斜杠与正斜杠必须编码为同一扁平目录名（对齐 pi 的
;; --${cwd.replace(/^[/\\]/, "").replace(/[/\\:]/g, "-")}-- ）
(check (encode-cwd-to-session-dirname "C:\\Users\\darcy\\git\\goldfish2")
  => "--C--Users-darcy-git-goldfish2--")
(check (encode-cwd-to-session-dirname "C:/Users/darcy/git/goldfish2")
  => "--C--Users-darcy-git-goldfish2--")
;; 盘符相对根路径（前导 \ 剥一位）
(check (encode-cwd-to-session-dirname "\\Users\\darcy")
  => "--Users-darcy--")

(check-report)
