(import (scheme base) (liii check) (liii goldcheck layer)
  (liii goldcheck layer-locate)
  (liii goldcheck layer-config))

;; domain-name-of-lib
;; 查询库名归属的域。多域命中时取最特异 pattern。
;; 配置用 JSON 的 Scheme 表示（对象 = alist，数组 = vector）。
;;
;; 语法
;; ----
;; (domain-name-of-lib domains lib)
;;
;; 参数
;; ----
;; domains : list of domain   layer-domains-parse 的结果
;; lib : string               库名
;;
;; 返回值
;; ------
;; string 或 #f
;; 域名；未登记返回 #f。

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
    (%cfg (%dom "goldfish" "goldfish/" '()
            (list "kernel" "liii base" "liii error")
            (list "std" "srfi srfi-1" "liii goldtool" "srfi srfi-113")
            (list "app" "liii string"))
      (%dom "common" "tools/common/" '("goldfish")
        (list "shared" "liii goldtool changed"))
      (%dom "check" "tools/check/" '("goldfish" "common")
        (list "tool" "liii goldcheck")))
  ) ;layer-domains-parse
)


;; 1. 域归属判定
(check (domain-name-of-lib %domains "srfi srfi-113") => "goldfish")

;; 2. 最特异 pattern 优先：goldfish 域在前的家族前缀 "liii goldtool"（rank 2）
;;    让位于 common 域的精确 "liii goldtool changed"（rank 3）
(check (domain-name-of-lib %domains "liii goldtool changed") => "common")

;; 3. 家族前缀继承：登记 "liii goldcheck" 则 "liii goldcheck layer" 同域同层
(check (domain-name-of-lib %domains "liii goldcheck layer") => "check")
(check (domain-name-of-lib %domains "liii string") => "goldfish")

;; 4. 未登记库返回 #f
(check (domain-name-of-lib %domains "liii csv") => #f)

;; 5. 尾通配符 "liii *"：兜底匹配任意 liii 库（rank 1）
(define %wild-domains
  (layer-domains-parse
    (%cfg (%dom "goldfish" "goldfish/" '()
            (list "app" "liii *"))))
)

(check (domain-name-of-lib %wild-domains "liii anything") => "goldfish")
(check (domain-name-of-lib %wild-domains "srfi srfi-1") => #f)

(check-report)
