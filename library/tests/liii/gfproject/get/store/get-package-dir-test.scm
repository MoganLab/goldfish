;; get-package-dir
;; 返回远程包安装目录 site-lisp/<org>/<name>/<version>；version 缺省 HEAD。
;;
;; 语法
;; ----
;; (get-package-dir package version)
;;
;; 参数
;; ----
;; package : gfproject-package?
;; 包元数据记录。
;;
;; version : string? or #f
;; 版本目录名；#f 或空串时使用 HEAD。
;;
;; 返回值
;; ----
;; path?
;; 安装目录路径。

(import (liii check) (liii path)
  (liii gfproject package) (liii gfproject get store)
) ;import

(check-set-mode! 'report-failed)

(define pkg (gfproject-package "highlight" "liii"))

(check-true (path-equals? (get-package-dir pkg "HEAD")
              (path-join (get-site-lisp-dir) "liii" "highlight" "HEAD")
            ) ;path-equals?
) ;check-true
(check-true (path-equals? (get-package-dir pkg #f)
              (path-join (get-site-lisp-dir) "liii" "highlight" "HEAD")
            ) ;path-equals?
) ;check-true
(check-true (path-equals? (get-package-dir pkg "v1.0.0")
              (path-join (get-site-lisp-dir) "liii" "highlight" "v1.0.0")
            ) ;path-equals?
) ;check-true
(check-catch 'type-error (get-package-dir '(("name" . "x")) "HEAD"))

(check-report)
