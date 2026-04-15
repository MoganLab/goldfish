(import (liii check) (liii os))


(check-set-mode! 'report-failed)


;; os-windows?
;; 判断当前操作系统是否为 Windows。
;;
;; 语法
;; ----
;; (os-windows?)
;;
;; 返回值
;; -----
;; boolean?
;; 如果是 Windows 系统返回 #t，否则返回 #f。


;; ; 基本功能测试
(when (os-windows?)
  (check (os-type) => "Windows")
) ;when


(check-report)
