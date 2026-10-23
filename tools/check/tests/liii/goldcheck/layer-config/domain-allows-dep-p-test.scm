(import (scheme base) (liii check) (liii goldcheck layer-config))

;; domain-allows-dep?
;; 闭包判定：dep-name 是否属于 domain 的直接或间接依赖。
;; 配置用 JSON 的 Scheme 表示。
;;
;; 语法
;; ----
;; (domain-allows-dep? domain domains dep-name)
;;
;; 参数
;; ----
;; domain : domain            被判定的域对象
;; domains : list of domain   layer-domains-parse 的结果
;; dep-name : string          依赖域名
;;
;; 返回值
;; ------
;; boolean
;; dep-name 在 domain 的依赖闭包中为 #t。

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

;; tool-x 只声明 common，经 common -> goldfish 传递后
;; import goldfish 域的库合法，check 域不合法
(define %domains
  (layer-domains-parse
    (%cfg (%dom "goldfish" "goldfish/" '() (list "app" "liii *" "srfi *"))
      (%dom "common" "tools/common/" '("goldfish")
        (list "shared" "liii goldtool-changed"))
      (%dom "tool-x" "tools/tool-x/" '("common") (list "l" "liii toolx*")))
  ) ;layer-domains-parse
)

(let ((tool-x (find-domain-by-name %domains "tool-x")))
  ;; 1. 直接依赖
  (check-true (domain-allows-dep? tool-x %domains "common"))
  ;; 2. 传递依赖自动允许
  (check-true (domain-allows-dep? tool-x %domains "goldfish"))
  ;; 3. 未声明的域拒绝
  (check-false (domain-allows-dep? tool-x %domains "check"))
)
