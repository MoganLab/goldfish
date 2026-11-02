;; domain-name-of
;; 获取域定义对象中的域名称。
;;
;; 语法
;; ----
;; (domain-name-of domain)
;;
;; 参数
;; ----
;; domain : domain   已解析的域定义对象
;;
;; 返回值
;; ------
;; string
;; 域的名称。

(import (scheme base) (liii check) (liii goldcheck layer-config))

(check-set-mode! 'report-failed)

;; 1. 获取域对象的 name 属性
(let ((dom '((name . "goldfish")
             (root . "goldfish/")
             (depends-on . #())
             (layers))))
  (check (domain-name-of dom) => "goldfish")
) ;let

(check-report)
