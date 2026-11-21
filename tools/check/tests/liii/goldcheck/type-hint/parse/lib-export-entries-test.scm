;; lib-export-entries
;; 从库解析信息中提取对外名到内部名的导出映射。
;;
;; 语法
;; ----
;; (lib-export-entries info)
;;
;; 参数
;; ----
;; info : list  库解析数据结构 alist
;;
;; 返回值
;; ------
;; list of pair
;; (external-name . internal-name) 映射列表。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

(let* ((path "tests/resources/type-hint/goldfish/liii/a7reexport.scm")
       (info (parse-library-file path))
       (entries (lib-export-entries info))
      ) ;
  (check (pair? entries) => #t)
) ;let

(check-report)
