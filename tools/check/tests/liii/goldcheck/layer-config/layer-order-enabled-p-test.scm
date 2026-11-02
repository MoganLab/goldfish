;; layer-order-enabled?
;; 判断配置中是否启用了 layer-order 检查规则。
;;
;; 语法
;; ----
;; (layer-order-enabled? cfg)
;;
;; 参数
;; ----
;; cfg : json-object   根配置对象
;;
;; 返回值
;; ------
;; boolean
;; 启用返回 #t，显式禁用返回 #f。

(import (scheme base) (liii check) (liii goldcheck layer-config))

(check-set-mode! 'report-failed)

;; 1. 默认未显式指定 enabled 字段时视为启用
(let ((cfg
        (list
          (cons "rules" (list (cons "layer-order" (list (cons "domains" '#())))))
        ) ;list
      ) ;cfg
     ) ;
  (check (layer-order-enabled? cfg) => #t)
) ;let

;; 2. 显式指定 enabled 为 #f 时返回 #f
(let ((cfg
        (list
          (cons "rules" (list (cons "layer-order" (list (cons "enabled" #f)))))
        ) ;list
      ) ;cfg
     ) ;
  (check (layer-order-enabled? cfg) => #f)
) ;let

;; 3. 显式指定 enabled 为 #t 时返回 #t
(let ((cfg
        (list
          (cons "rules" (list (cons "layer-order" (list (cons "enabled" #t)))))
        ) ;list
      ) ;cfg
     ) ;
  (check (layer-order-enabled? cfg) => #t)
) ;let

(check-report)
