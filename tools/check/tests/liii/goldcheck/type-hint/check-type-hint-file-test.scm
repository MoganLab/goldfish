;; check-type-hint-file
;; 对任意源码文件做 type-hint 检查的顶层入口：
;; hint 目录下的文件走 check-type-hint（结构校验 + 原库推导），
;; 其余 define-library 文件按 import 的 hint 检查函数体调用。
;;
;; 语法
;; ----
;; (check-type-hint-file file)
;;
;; 参数
;; ----
;; file : string  源码文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 违规列表，每项形如 (list 'type-hint rel-path line msg [expr])。

(import (scheme base)
  (liii check)
  (liii string)
  (liii os)
  (liii goldcheck type-hint)
) ;import

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 1. hint 目录下的文件：走 check-type-hint 的结构校验
;;    badsig.scm 是签名语法违规 fixture，结构违规应被报告
(let ((viols (check-type-hint-file (resource-file "goldfish/hint/liii/badsig.scm"))))
  (check-true (pair? viols))
) ;let

;; 2. 非 hint 的普通源码文件：按 import 的 hint 检查函数体
(let ((viols (check-type-hint-file (resource-file "goldfish/liii/filedemo.scm"))))
  (check (length viols) => 1)
  (check-true (string-contains? (cadddr (car viols)) "argument 1 of <"))
) ;let

;; 3. 非 define-library 文件返回 '()
(check (check-type-hint-file (resource-file "script.scm")) => '())

(check-report)
