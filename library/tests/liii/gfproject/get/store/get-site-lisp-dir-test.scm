;; get-site-lisp-dir
;; 返回用户级源码缓存目录 ~/.gf/site-lisp。
;;
;; 语法
;; ----
;; (get-site-lisp-dir)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; path?
;; ~/.gf/site-lisp 目录路径。

(import (liii check) (liii path) (liii gfproject get store))

(check-set-mode! 'report-failed)

(check-true (path-equals? (get-site-lisp-dir)
              (path-join (path-home) ".gf" "site-lisp")
            ) ;path-equals?
) ;check-true

(check-report)
