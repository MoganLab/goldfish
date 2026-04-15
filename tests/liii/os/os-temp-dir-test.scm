(import (liii check)
  (liii os)
  (liii string)
) ;import


(check-set-mode! 'report-failed)


;; os-temp-dir
;; 返回系统临时目录的路径。
;;
;; 语法
;; ----
;; (os-temp-dir)
;;
;; 返回值
;; -----
;; string?
;; 返回系统临时目录的完整路径。
;;
;; 说明
;; ----
;; 1. 在 Linux 系统上通常返回 "/tmp"
;; 2. 在 Windows 系统上通常返回类似 "C:\\Users\\...\\Temp" 的路径


;; ; 基本功能测试
(when (os-windows?)
  (check (string-starts? (os-temp-dir) "C:")
    =>
    #t
  ) ;check
) ;when


(when (os-linux?)
  (check (os-temp-dir) => "/tmp")
) ;when


;; ; 验证返回值非空
(check-false (string-null? (os-temp-dir))
) ;check-false


(check-report)
