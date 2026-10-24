(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii goldcheck layer)
  (liii goldcheck layer-config)
  (liii goldcheck layer-locate)
) ;import

;; check-layer-order
;; 对单文件执行 layer-order 检查。三步判定：
;; 同域只许向下依赖；跨域须经 depends-on 声明（闭包）；
;; 其余（自库未登记或 import 未登记库）报 unknown。
;; 配置用 JSON 的 Scheme 表示（对象 = alist，数组 = vector）。
;;
;; 语法
;; ----
;; (check-layer-order file-path cfg)
;;
;; 参数
;; ----
;; file-path : string  文件路径（目录结构决定自库归属）
;; cfg : alist         检查配置（rules.layer-order）
;;
;; 返回值
;; ------
;; list of violation
;; 每项形如 (list 'layer-order rel-path detail)。

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

(define (%cfg-on . domains)
  (list
    (cons "rules"
      (list
        (cons "layer-order"
          (list (cons "enabled" #t) (cons "domains" (list->vector domains)))
        ) ;cons
      ) ;list
    ) ;cons
  ) ;list
) ;define

(define %cfg
  (%cfg-on (%dom "goldfish"
             "goldfish/"
             '()
             (list "kernel" "liii base" "liii error")
             (list "std" "srfi srfi-1" "srfi srfi-113")
             (list "app" "liii path" "liii os" "liii string" "liii json" "liii hash-table")
           ) ;%dom
    (%dom "common"
      "tools/common/"
      '("goldfish")
      (list "shared" "liii goldtool changed")
    ) ;%dom
    (%dom "check"
      "tools/check/"
      '("goldfish" "common")
      (list "tool" "liii goldcheck")
    ) ;%dom
  ) ;%cfg-on
) ;define

;; check 域未声明 common（其余同 %cfg），用于跨域未声明的负例

(define %cfg-no-common
  (%cfg-on (%dom "goldfish"
             "goldfish/"
             '()
             (list "kernel" "liii base" "liii error")
             (list "std" "srfi srfi-1" "srfi srfi-113")
             (list "app" "liii path" "liii os" "liii string" "liii json" "liii hash-table")
           ) ;%dom
    (%dom "common"
      "tools/common/"
      '("goldfish")
      (list "shared" "liii goldtool changed")
    ) ;%dom
    (%dom "check" "tools/check/" '("goldfish") (list "tool" "liii goldcheck"))
  ) ;%cfg-on
) ;define

;; enabled: #f 的关闭配置

(define %cfg-off
  (list
    (cons "rules"
      (list
        (cons "layer-order"
          (list (cons "enabled" #f)
            (cons "domains"
              (list->vector (list (%dom "goldfish" "goldfish/" '() (list "app" "liii string" "liii hash-table"))))
            ) ;cons
          ) ;list
        ) ;cons
      ) ;list
    ) ;cons
  ) ;list
) ;define

;; 在临时目录模拟仓库目录结构（path->lib-name 只认路径段）

(define %tmp-root (path->string (path-join (path-temp-dir) "gf-layer-e2e")))

(define (%mkdirs p)
  (if (path-dir? p) #f (begin (%mkdirs (path-parent p)) (path-mkdir p)))
) ;define

(define (%mk rel content)
  (let ((full (string-append %tmp-root "/" rel)))
    (%mkdirs (path-parent (path full)))
    (call-with-port (open-output-file full) (lambda (p) (display content p)))
    full
  ) ;let
) ;define

(define (%lib-header . libs)
  (string-append "(define-library (x)\n  (import "
    (apply string-append (map (lambda (l) (string-append "(" l ")\n    ")) libs))
    ") ;import\n  (begin\n  ) ;begin\n) ;define-library\n"
  ) ;string-append
) ;define

;; 1. 同域向下 + 跨域已声明 -> 零违规
(let ((f (%mk "tools/check/liii/goldcheck/path.scm"
           (%lib-header "scheme base" "liii path" "liii os" "liii string")
         ) ;%mk
      ) ;f
     ) ;
  (check (check-layer-order f %cfg) => '())
) ;let

;; 1b. 同层互依放行（app -> app），向上才违规
(let ((f (%mk "tools/check/liii/goldcheck/config.scm"
          (%lib-header "scheme base" "liii json" "liii goldcheck path"))))
  (check (check-layer-order f %cfg) => '())
)

;; 2. 同域向上依赖：std 层的 srfi 库 import app 层 -> 抓到倒挂
(let* ((f (%mk "goldfish/srfi/srfi-113.scm"
            (%lib-header "scheme base" "liii hash-table" "liii error")
          ) ;%mk
       ) ;f
       (viol (check-layer-order f %cfg))
       (details (map caddr viol))
      ) ;
  (check (length viol) => 1)
  (check-true (string-contains? (caddr (car viol)) "hash-table"))
) ;let*

;; 2b. 裸顶层 import（无 define-library 的扩展插件式文件）同样受检：
;; std 层裸文件 import app 层 -> 抓到倒挂
(let* ((f (%mk "goldfish/srfi/srfi-113.scm"
            "(import (scheme base) (liii hash-table))\n\n(define (x) 1)\n"))
       (viol (check-layer-order f %cfg))
      ) ;
  (check (length viol) => 1)
  (check-true (and (string-contains? (caddr (car viol)) "hash-table") #t))
) ;let*

;; 3. 跨域已声明 -> 零违规；未声明 -> 1 条 cross-domain 违规
(let ((f (%mk "tools/check/liii/goldcheck.scm"
           (%lib-header "scheme base" "liii goldtool changed" "liii string")
         ) ;%mk
      ) ;f
     ) ;
  (check (check-layer-order f %cfg) => '())
  (let* ((ds (layer-domains-parse %cfg-no-common))
         (check-dom (find-domain-by-name ds "check"))
         (details (map caddr (check-layer-order f %cfg-no-common)))
        ) ;
    (check (domain-name-of-lib ds "liii goldcheck") => "check")
    (check (domain-name-of-lib ds "liii goldtool changed") => "common")
    (check-false (domain-allows-dep? check-dom ds "common"))
    (check (length details) => 1)
    (check-true (string-contains? (car details) "goldtool"))
  ) ;let*
) ;let

;; 4. tests/ 路径豁免：消费者不受检
(let ((f (%mk "tools/check/tests/liii/goldcheck/x.scm"
           (%lib-header "scheme base" "liii string")
         ) ;%mk
      ) ;f
     ) ;
  (check (check-layer-order f %cfg) => '())
) ;let

;; 4b. 域外文件（不在任何域 root 下）-> 跳过
(let ((f (%mk "demo/not-in-domain.scm"
          (%lib-header "scheme base" "liii string"))))
  (check (check-layer-order f %cfg) => '())
)

;; 5. scheme 基座文件跳过
(let ((f (%mk "goldfish/scheme/base.scm" (%lib-header "scheme base"))))
  (check (check-layer-order f %cfg) => '())
) ;let

;; 6. 自库未登记：common 域下新增未登记库 -> lib not registered
(let* ((f (%mk "tools/common/liii/goldtool-new.scm"
            (%lib-header "scheme base" "liii string")
          ) ;%mk
       ) ;f
       (viol (check-layer-order f %cfg))
       (details (map caddr viol))
      ) ;
  (check (length viol) => 1)
  (check-true (string-contains? (car details) "goldtool-new"))
) ;let*

;; 7. import 未登记库 -> unknown lib
(let* ((f (%mk "tools/check/liii/goldcheck.scm" (%lib-header "scheme base" "liii csv"))
       ) ;f
       (viol (check-layer-order f %cfg))
       (details (map caddr viol))
      ) ;
  (check (length viol) => 1)
  (check-true (string-contains? (car details) "liii csv"))
) ;let*

;; 8. enabled: #f -> 规则关闭返回 '()
(let ((f (%mk "goldfish/srfi/srfi-113.scm" (%lib-header "scheme base" "liii hash-table"))
      ) ;f
     ) ;
  (check (check-layer-order f %cfg-off) => '())
) ;let

;; ---- 清理临时目录 ---------------------------------------------------
(let ((files '("tools/check/liii/goldcheck/path.scm"
               "goldfish/srfi/srfi-113.scm" "tools/check/liii/goldcheck.scm"
               "tools/check/tests/liii/goldcheck/x.scm"
               "goldfish/scheme/base.scm" "tools/common/liii/goldtool-new.scm")
      ) ;files
     ) ;
  (for-each
    (lambda (rel) (path-unlink (path (string-append %tmp-root "/" rel)) #t))
    files
  ) ;for-each
  (for-each
    (lambda (d) (path-rmdir (path (string-append %tmp-root "/" d))))
    '("tools/check/liii/goldcheck" "tools/check/tests/liii/goldcheck"
      "tools/check/tests/liii" "tools/check/tests" "tools/check/liii"
      "tools/check" "goldfish/srfi" "goldfish/scheme" "goldfish"
      "tools/common/liii" "tools/common" "tools")
  ) ;for-each
  (path-rmdir (path %tmp-root))
) ;let

(check-report)
