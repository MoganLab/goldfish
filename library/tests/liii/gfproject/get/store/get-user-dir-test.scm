;; get-user-dir
;; 返回用户级配置目录 ~/.gf。
;;
;; 语法
;; ----
;; (get-user-dir)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; path?
;; ~/.gf 目录路径。

(import (liii check) (liii path) (liii gfproject get store))

(check-set-mode! 'report-failed)

(check-true (path-equals? (get-user-dir) (path-join (path-home) ".gf")))

(check-report)
