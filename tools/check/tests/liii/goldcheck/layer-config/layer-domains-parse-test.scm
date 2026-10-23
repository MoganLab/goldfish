(import (scheme base) (liii check) (liii goldcheck layer-config))

;; layer-domains-parse
;; 从 cfg 的 rules.layer-order.domains 解析域表并自检：
;; depends-on 引用存在、域名唯一、域依赖图无环。
;; cfg 用 JSON 的 Scheme 表示（对象 = alist，数组 = vector）。
;;
;; 语法
;; ----
;; (layer-domains-parse cfg)
;;
;; 参数
;; ----
;; cfg : alist 或 #f
;; 检查配置对象。
;;
;; 返回值
;; ------
;; list of domain
;; 域列表，每域为 ((name . str) (root . str) (depends-on . vector) (layers . list))。

(check-set-mode! 'report-failed)

;; JSON 表示构造助手：(%dom 名 root 依赖域 (层名 库...) ...)
(define (%dom name root deps . layers)
  (list (cons "name" name)
    (cons "root" root)
    (cons "depends-on" (list->vector deps))
    (cons "layers"
      (list->vector
        (map
          (lambda (l)
            (list (cons "name" (car l))
              (cons "libs" (list->vector (cdr l)))))
          layers))))
) ;define

;; (%cfg 域...) -> ((rules . ((layer-order . ((domains . #(...))))))
(define (%cfg . domains)
  (list (cons "rules"
    (list (cons "layer-order"
      (list (cons "domains" (list->vector domains))))))
) ;list
) ;define

;; 1. 合法配置解析出域列表
(let* ((cfg (%cfg (%dom "goldfish" "goldfish/" '() (list "app" "liii *"))))
       (domains (layer-domains-parse cfg))
      ) ;
  (check (length domains) => 1)
  (check (find-domain-by-name domains "goldfish") => (car domains))
)

;; 2. depends-on 引用不存在的域 -> value-error
(check-catch 'value-error
  (layer-domains-parse
    (%cfg (%dom "a" "a/" '("ghost") (list "l" "liii a")))
  ) ;layer-domains-parse
) ;check-catch

;; 3. 域依赖图成环 -> value-error
(check-catch 'value-error
  (layer-domains-parse
    (%cfg (%dom "a" "a/" '("b") (list "l" "liii a"))
      (%dom "b" "b/" '("a") (list "l" "liii b")))
  ) ;layer-domains-parse
) ;check-catch

;; 4. 未配置 layer-order 或 cfg 为 #f 时返回空表
(check (layer-domains-parse #f) => '())
(check (layer-domains-parse '((rules . ()))) => '())
