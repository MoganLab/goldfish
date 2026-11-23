;; get-lock-path
;; 返回用户级 lock 文件路径 ~/.gf/gfproject.lock。
;;
;; 语法
;; ----
;; (get-lock-path)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; path?
;; lock 文件路径。

(import (liii check) (liii path) (liii gfproject get store))

(check-set-mode! 'report-failed)

(check-true (path-equals? (get-lock-path)
              (path-join (path-home) ".gf" "gfproject.lock")
            ) ;path-equals?
) ;check-true

(check-report)
