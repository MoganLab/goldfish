;; reexport-owner
;; 确定再导出符号的唯一所有者库。
;;
;; 语法
;; ----
;; (reexport-owner lib-name external-name . roots)
;;
;; 参数
;; ----
;; lib-name : list  再导出库名
;; external-name : symbol  对外导出名
;; roots : list  可选源码根列表
;;
;; 返回值
;; -------
;; list
;; 唯一所有者、not-exported 或 indeterminate。

(import (scheme base) (liii check) (liii goldcheck analysis))

(check-set-mode! 'report-failed)

(check (car (reexport-owner '(analysis reexport) 'external-name (list "tests/resources"))
       ) ;car
  =>
  'owner
) ;check
(check (car (reexport-owner '(analysis ambiguous) 'shared-name (list "tests/resources"))
       ) ;car
  =>
  'indeterminate
) ;check

(check-report)
