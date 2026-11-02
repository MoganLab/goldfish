;; %domain-for-path
;; 按 root 段序列在路径段中的位置定位文件所属域。
;;
;; 语法
;; ----
;; (%domain-for-path domains path)
;;
;; 参数
;; ----
;; domains : list of domain   已解析的域定义列表
;; path : string              文件路径
;;
;; 返回值
;; ------
;; domain 或 #f
;; 命中的域对象；未命中返回 #f。

(import (scheme base)
  (liii check)
  (liii goldcheck layer)
  (liii goldcheck layer-config)
  (liii goldcheck layer-locate)
) ;import

(check-set-mode! 'report-failed)

(define (%dom name root deps . layers)
  (list (cons "name" name)
    (cons "root" root)
    (cons "depends-on" (list->vector deps))
    (cons "layers"
      (list->vector
        (map
          (lambda (l) (list (cons "name" (car l)) (cons "libs" (list->vector (cdr l)))))
          layers
        ) ;map
      ) ;list->vector
    ) ;cons
  ) ;list
) ;define

(define %domains
  (layer-domains-parse
    (list
      (cons "rules"
        (list
          (cons "layer-order"
            (list
              (cons "domains"
                (vector (%dom "goldfish" "goldfish/" '() (list "core" "scheme base"))
                  (%dom "tools" "tools/" '("goldfish") (list "tool" "liii goldcheck"))
                ) ;vector
              ) ;cons
            ) ;list
          ) ;cons
        ) ;list
      ) ;cons
    ) ;list
  ) ;layer-domains-parse
) ;define

;; 1. 命中 goldfish 域
(let ((dom (%domain-for-path %domains "goldfish/liii/base.scm")))
  (check (and dom (domain-name-of dom)) => "goldfish")
) ;let

;; 2. 命中 tools 域
(let ((dom (%domain-for-path %domains "tools/check/liii/goldcheck.scm")))
  (check (and dom (domain-name-of dom)) => "tools")
) ;let

;; 3. 未命中的路径返回 #f
(check (%domain-for-path %domains "unknown/path/test.scm") => #f)

(check-report)
