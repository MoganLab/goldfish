;; domain-layers-of
;; 获取域定义对象中的分层结构列表。
;;
;; 语法
;; ----
;; (domain-layers-of domain)
;;
;; 参数
;; ----
;; domain : domain   已解析的域定义对象
;;
;; 返回值
;; ------
;; list
;; 分层定义对象列表。

(import (scheme base) (liii check) (liii goldcheck layer-config))

(check-set-mode! 'report-failed)

;; 1. 获取域对象的 layers 属性
(let* ((layers (list '((name . "core") (libs . #("scheme base")))))
       (dom (list (cons 'name "goldfish")
              (cons 'root "goldfish/")
              (cons 'depends-on '#())
              (cons 'layers layers)
            ) ;list
       ) ;dom
      ) ;
  (check (domain-layers-of dom) => layers)
) ;let*

(check-report)
