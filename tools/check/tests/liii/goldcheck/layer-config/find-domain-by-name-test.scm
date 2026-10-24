(import (scheme base) (liii check) (liii goldcheck layer-config))

;; find-domain-by-name
;; 按域名查找域对象。配置用 JSON 的 Scheme 表示。
;;
;; 语法
;; ----
;; (find-domain-by-name domains name)
;;
;; 参数
;; ----
;; domains : list of domain   layer-domains-parse 的结果
;; name : string              域名
;;
;; 返回值
;; ------
;; domain 或 #f

(check-set-mode! 'report-failed)

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

(define (%cfg . domains)
  (list (cons "rules"
    (list (cons "layer-order"
      (list (cons "domains" (list->vector domains))))))
) ;list
) ;define

(define %domains
  (layer-domains-parse
    (%cfg (%dom "goldfish" "goldfish/" '() (list "app" "liii *"))
      (%dom "common" "tools/common/" '("goldfish")
        (list "shared" "liii goldtool changed")))
  ) ;layer-domains-parse
)

;; 1. 找到存在的域
(check-true (and (find-domain-by-name %domains "common") #t))

;; 2. 不存在的域返回 #f
(check-false (find-domain-by-name %domains "ghost"))

(check-report)
