(import (scheme base) (liii check) (liii goldcheck layer)
  (liii goldcheck layer-locate)
  (liii goldcheck layer-config))

;; layer-name-of-lib
;; 查询库名在域表中命中的层名。多域命中时取最特异 pattern。
;; 配置用 JSON 的 Scheme 表示（对象 = alist，数组 = vector）。
;;
;; 语法
;; ----
;; (layer-name-of-lib domains lib)
;;
;; 参数
;; ----
;; domains : list of domain   layer-domains-parse 的结果
;; lib : string               库名，如 "liii string"
;;
;; 返回值
;; ------
;; string 或 #f
;; 层名；未登记返回 #f。

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
        (list "shared" "liii goldtool-changed"))
      (%dom "check" "tools/check/" '("goldfish" "common")
        (list "tool" "liii goldcheck")))
  ) ;layer-domains-parse
)

;; 1. 精确 pattern 命中
(check (layer-name-of-lib %domains "liii base") => "kernel")
(check (layer-name-of-lib %domains "liii error") => "kernel")

;; 2. 通配 pattern 命中
(check (layer-name-of-lib %domains "srfi srfi-1") => "std")
(check (layer-name-of-lib %domains "liii string") => "app")

;; 3. 最特异优先："liii goldcheck layer" 同时命中
;;    goldfish 域 "liii *"（通配兜底）与 check 域 "liii goldcheck"（更长通配）
(check (layer-name-of-lib %domains "liii goldcheck layer") => "tool")

;; 4. 未登记库返回 #f
(check (layer-name-of-lib %domains "liii csv") => #f)
