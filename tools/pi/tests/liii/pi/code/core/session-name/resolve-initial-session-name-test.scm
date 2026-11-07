;; resolve-initial-session-name
;; 解析新会话的初始名称。
;;
;; 语法
;; ----
;; (resolve-initial-session-name parsed cfg cwd)
;;
;; 参数
;; ----
;; parsed : list?
;; 命令行解析后的参数 alist。
;;
;; cfg : list?
;; Agent 配置 alist。
;;
;; cwd : string
;; 当前工作目录。
;;
;; 返回值
;; ----
;; (or string #f)
;; 初始会话名称，未指定时返回 #f。

(import (liii check) (liii pi code core session-name))

(check-set-mode! 'report-failed)

;; 1. 显式指定 --name 时返回该名称
(check (resolve-initial-session-name '((name . "my-session")) '() ".")
  =>
  "my-session"
) ;check

;; 2. 指定 --no-session 时不解析会话名称
(check (resolve-initial-session-name '((no-session . #t))
         '((agent-dir . "/tmp/pi-agent-test"))
         "."
       ) ;resolve-initial-session-name
  =>
  #f
) ;check

(check-report)
