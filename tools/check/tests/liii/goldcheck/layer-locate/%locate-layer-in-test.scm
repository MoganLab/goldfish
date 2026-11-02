;; %locate-layer-in
;; 在指定域内检索库名所在层（支持家族前缀继承）。
;;
;; 语法
;; ----
;; (%locate-layer-in domains domain lib)
;;
;; 参数
;; ----
;; domains : list of domain   已解析的域定义列表
;; domain : domain            目标域对象
;; lib : string               待检索的库名
;;
;; 返回值
;; ------
;; pair 或 #f
;; 命中的 (layer . idx) 对；未命中返回 #f。

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
                          (list "std" "srfi srfi-1")
                        ) ;%dom
                ) ;vector
              ) ;cons
            ) ;list
          ) ;cons
        ) ;list
      ) ;cons
    ) ;list
  ) ;layer-domains-parse
) ;define

(define %domain (car %domains))

;; 1. 命中首层（index 为 0）
(let ((hit (%locate-layer-in %domains %domain "liii base")))
  (check (and hit (layer-name-of (car hit))) => "kernel")
  (check (and hit (cdr hit)) => 0)
) ;let

;; 2. 命中次层（index 为 1）
(let ((hit (%locate-layer-in %domains %domain "srfi srfi-1")))
  (check (and hit (layer-name-of (car hit))) => "std")
  (check (and hit (cdr hit)) => 1)
) ;let

;; 3. 家族前缀继承命中
(let ((hit (%locate-layer-in %domains %domain "liii base extra")))
  (check (and hit (layer-name-of (car hit))) => "kernel")
  (check (and hit (cdr hit)) => 0)
) ;let

;; 4. 未命中返回 #f
(check (%locate-layer-in %domains %domain "unknown-lib") => #f)

(check-report)
