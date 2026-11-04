;; os-linux?
;; 判断当前操作系统是否为 Linux。
;;
;; 语法
;; ----
;; (os-linux?)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; boolean?
;; 如果是 Linux 系统返回 #t，否则返回 #f。

;; ; 基本功能测试

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

(when (os-linux?)
  (check (os-type) => "Linux")
) ;when

(check-report)
