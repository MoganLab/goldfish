(import (liii check)
        (liii os)
        (liii sys)
) ;import

(check-set-mode! 'report-failed)

;; access
;; 检查文件的访问权限。
;;
;; 语法
;; ----
;; (access path mode)
;;
;; 参数
;; ----
;; path : string?
;; 要检查的文件路径。
;;
;; mode : symbol?
;; 访问模式，可以是 'F_OK（存在）、'R_OK（可读）、
;; 'W_OK（可写）、'X_OK（可执行）。
;;
;; 返回值
;; -----
;; boolean?
;; 如果有指定权限返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 检查当前进程对指定文件的访问权限。

;;; 基本功能测试
(when (and (os-linux?) (not (string=? "root" (getlogin))))
  (check-true (access "/root" 'F_OK))
  (check-false (access "/root" 'R_OK))
  (check-false (access "/root" 'W_OK))
  (check-true (access (executable) 'X_OK))
) ;when

(check-report)
