;; domain-deps-of
;; 获取域定义对象中的依赖域名 vector。
;;
;; 语法
;; ----
;; (domain-deps-of domain)
;;
;; 参数
;; ----
;; domain : domain   已解析的域定义对象
;;
;; 返回值
;; ------
;; vector
;; 依赖的域名向量。

(import (scheme base) (liii check) (liii goldcheck layer-config))

(check-set-mode! 'report-failed)

;; 1. 获取域对象的 depends-on 属性
(let ((dom '((name . "tools")
             (root . "tools/")
             (depends-on . #("goldfish"))
             (layers))
      ) ;dom
     ) ;
  (check (domain-deps-of dom) => '#("goldfish"))
) ;let

(check-report)
