;; lib-import-clauses
;; 从库解析信息中提取原始 import 子句。
;;
;; 语法
;; ----
;; (lib-import-clauses info)
;;
;; 参数
;; ----
;; info : list  库解析数据结构 alist
;;
;; 返回值
;; ------
;; list
;; 原始 import 子句列表。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

(let* ((path "tests/resources/type-hint/goldfish/liii/a7dep.scm")
       (info (parse-library-file path))
      ) ;
  (check (pair? (lib-import-clauses info)) => #t)
) ;let*

(check-report)
