;; check-dep-hints
;; 检查目标库 import 闭包中各依赖库 hint 的结构违规（第二层检查）。
;;
;; 语法
;; ----
;; (check-dep-hints check-hint file)
;;
;; 参数
;; ----
;; check-hint : procedure  第一层检查函数（check-type-hint）
;; file       : string     被查源码/hint 文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 违规归属被查文件，消息内嵌来源 hint 文件位置与行号。

(import (scheme base)
  (liii check)
  (liii os)
  (liii string)
  (liii goldcheck type-hint)
  (liii goldcheck type-hint target)
)

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 1. 依赖 hint 结构违规被报告，归属被查文件
(let ((viols
        (check-dep-hints check-type-hint
          (resource-file "goldfish/liii/depbaduser.scm")
        ) ;check-dep-hints
      ) ;viols
    )
  ;; 依赖 hint 的两个结构违规：invalid signature + arity 不匹配
  (check (length viols) => 2)
  (check-true
    (string-contains? (cadddr (car viols)) "goldfish/hint/liii/depbadsig.scm")
  ) ;check-true
  (check-true (string-contains? (cadddr (car viols)) "invalid signature"))
) ;let

;; 2. 无库信息（非 define-library 文件）返回 '()
(check
  (check-dep-hints check-type-hint (resource-file "script.scm"))
  => '()
) ;check

(check-report)
