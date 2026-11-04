;; getlogin
;; 获取当前登录用户的用户名。
;;
;; 语法
;; ----
;; (getlogin)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; string? 或 #f
;; 返回登录用户名；无法获取（如 CI 环境无控制会话）时返回 #f。
;;
;; 说明
;; ----
;; Windows 平台读取环境变量 USERNAME，
;; POSIX 平台调用 C 库 getlogin。

(import (liii check) (liii os) (liii string))

(check-set-mode! 'report-failed)

;; 返回非空字符串或 #f（CI 等无登录会话的环境）
(let ((login (getlogin)))
  (check-true
    (or (not login) (and (string? login) (not (string-null? login))))
  ) ;check-true
) ;let

(check-report)
