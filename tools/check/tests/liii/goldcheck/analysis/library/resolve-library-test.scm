;; resolve-library
;; 从库名解析源码文件并读取对外导出名。
;;
;; 语法
;; ----
;; (resolve-library lib-name . roots)
;;
;; 参数
;; ----
;; lib-name : list  库名符号列表
;; roots : list  可选源码根列表
;;
;; 返回值
;; -------
;; list
;; resolved 时返回 (resolved file exports)，否则返回 indeterminate。

(import (scheme base) (liii check) (liii goldcheck analysis))

(check-set-mode! 'report-failed)

(let ((resolved (resolve-library '(analysis first) (list "tests/resources"))))
  (check (car resolved) => 'resolved)
  (check-true (pair? (member 'local-name (caddr resolved))))
) ;let

(check-report)
