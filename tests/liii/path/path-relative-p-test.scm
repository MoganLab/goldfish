(import (liii check)
  (liii path)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-relative?
;; 判断路径是否为相对路径。
;;
;; 语法
;; ----
;; (path-relative? path-value)
;;
;; 参数
;; ----
;; path-value : path-value | string?
;; 要检查的路径。
;;
;; 返回值
;; ----
;; boolean
;; 当路径为相对路径时返回 #t，否则返回 #f。

;; 基本相对路径测试
(check-true (path-relative? (path)))
(check-true (path-relative? (path "relative.txt"))
) ;check-true
(check-false (path-relative? (path-of-drive #\C))
) ;check-false

(when (not (os-windows?))
  (check-false (path-relative? (path "/tmp/demo.txt"))
  ) ;check-false
) ;when

(check-false (path-relative? (path-home))
) ;check-false
(check-false (path-relative? (path-temp-dir))
) ;check-false

(check-report)
