;; domain-root-of
;; 获取域定义对象中的根目录相对路径。
;;
;; 语法
;; ----
;; (domain-root-of domain)
;;
;; 参数
;; ----
;; domain : domain   已解析的域定义对象
;;
;; 返回值
;; ------
;; string
;; 域的根目录相对路径。

(import (scheme base) (liii check) (liii goldcheck layer-config))

(check-set-mode! 'report-failed)

;; 1. 获取域对象的 root 属性
(let ((dom '((name . "goldfish")
             (root . "goldfish/")
             (depends-on . #())
             (layers))))
  (check (domain-root-of dom) => "goldfish/")
) ;let

(check-report)
