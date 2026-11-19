;; paste-registry?
;; 判断给定对象是否为大段粘贴注册表实例。
;;
;; 语法
;; ----
;; (paste-registry? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为粘贴注册表实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui editor paste)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-paste-registry)))
  (check-true (paste-registry? reg))
) ;let

(check-false (paste-registry? "not-a-registry"))
(check-false (paste-registry? 123))

(check-report)
