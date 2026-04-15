(import (liii check) (liii os))


(check-set-mode! 'report-failed)


;; os-macos?
;; 判断当前操作系统是否为 macOS。
;;
;; 语法
;; ----
;; (os-macos?)
;;
;; 返回值
;; -----
;; boolean?
;; 如果是 macOS 系统返回 #t，否则返回 #f。


;; ; 基本功能测试
(when (os-macos?)
  (check (os-type) => "Darwin")
) ;when


(check-report)
