(import (liii check)
  (liii os)
  (liii string)
) ;import


(check-set-mode! 'report-failed)


;; getcwd
;; 获取当前工作目录。
;;
;; 语法
;; ----
;; (getcwd)
;;
;; 返回值
;; -----
;; string?
;; 返回当前工作目录的完整路径。
;;
;; 说明
;; ----
;; 返回进程当前的工作目录，返回值非空字符串。


;; ; 基本功能测试
(check-false (string-null? (getcwd)))


(check-report)
