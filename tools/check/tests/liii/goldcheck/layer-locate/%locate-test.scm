;; %locate
;; 按特异性穷举匹配，检索库名在域表中首个命中的 (domain layer idx)。
;;
;; 语法
;; ----
;; (%locate domains lib)
;;
;; 参数
;; ----
;; domains : list of domain   已解析的域定义列表
;; lib : string               待检索的库名
;;
;; 返回值
;; ------
;; list 或 #f
;; 命中的 (domain layer idx) 三元表；未命中返回 #f。

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
                (vector (%dom "goldfish"
                          "goldfish/"
                          '()
                          (list "kernel" "liii base" "liii error")
                          (list "app" "liii *")
                        ) ;%dom
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

;; 1. 精确命中 rank 3
(let ((loc (%locate %domains "liii base")))
  (check (and loc (domain-name-of (car loc))) => "goldfish")
  (check (and loc (layer-name-of (cadr loc))) => "kernel")
  (check (and loc (caddr loc)) => 0)
) ;let

;; 2. 尾通配命中 rank 1
(let ((loc (%locate %domains "liii string")))
  (check (and loc (domain-name-of (car loc))) => "goldfish")
  (check (and loc (layer-name-of (cadr loc))) => "app")
  (check (and loc (caddr loc)) => 1)
) ;let

;; 3. 未登记库名返回 #f
(check (%locate %domains "unknown-lib") => #f)

(check-report)
