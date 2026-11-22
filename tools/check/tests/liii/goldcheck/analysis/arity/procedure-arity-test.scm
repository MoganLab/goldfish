;; procedure-arity
;; 返回同库定义过程的固定、可选、rest 或关键字参数描述。
;;
;; 语法
;; ----
;; (procedure-arity lib-name name . roots)
;;
;; 参数
;; ----
;; lib-name : list  库名
;; name : symbol 过程名
;; roots : list 可选源码根列表
;;
;; 返回值
;; -------
;; list
;; fixed、optional、rest、keyword 或 indeterminate。

(import (scheme base) (liii check) (liii goldcheck analysis))

(check-set-mode! 'report-failed)

(let ((roots (list "tests/resources")))
  (check (procedure-arity '(analysis first) 'local-name roots) => '(fixed 1 1))
  (check (procedure-arity '(analysis first) 'optional-name roots)
    =>
    '(optional 1 2 #t)
  ) ;check
  (check (procedure-arity '(analysis first) 'rest-name roots) => '(rest 1))
  (check (procedure-arity '(analysis first) 'keyword-name roots)
    =>
    '(keyword 1 1 #t)
  ) ;check
) ;let

(check-report)
