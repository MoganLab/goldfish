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
            (list "std" "srfi srfi-1" "srfi srfi-113")
            (list "app" "liii *"))
      (%dom "common" "tools/common/" '("goldfish")
        (list "shared" "liii goldtool changed"))
      (%dom "check" "tools/check/" '("goldfish" "common")
        (list "tool" "liii goldcheck")))
  ) ;layer-domains-parse
)

;; 1. 域归属判定
(check (domain-name-of-lib %domains "srfi srfi-113") => "goldfish")
(check (domain-name-of-lib %domains "liii goldtool changed") => "common")

;; 2. 最特异 pattern 优先于 goldfish 域的 "liii *" 兜底
(check (domain-name-of-lib %domains "liii goldcheck") => "check")
(check (domain-name-of-lib %domains "liii string") => "goldfish")

;; 3. 未被任何 pattern 命中的库返回 #f
;;    注意："liii *" 是 family 通配兜底，会匹配任意 "liii xxx"。
(check (domain-name-of-lib %domains "csv foo") => #f)
