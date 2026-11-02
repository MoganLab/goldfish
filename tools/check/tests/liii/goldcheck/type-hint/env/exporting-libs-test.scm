;; exporting-libs
;; 从依赖闭包中按广度优先顺序筛选出导出了指定符号的库列表。
;;
;; 语法
;; ----
;; (exporting-libs name closure)
;;
;; 参数
;; ----
;; name : symbol   函数或变量名符号
;; closure : list  依赖闭包列表 ((深度 . 库信息) ...)
;;
;; 返回值
;; ------
;; list of pair
;; 导出了该符号的 (深度 . 库信息) 条目列表。

(import (scheme base)
  (liii check)
  (liii os)
  (liii goldcheck type-hint parse)
  (liii goldcheck type-hint env)
) ;import

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

(let* ((f (resource-file "goldfish/liii/a7reexport.scm"))
       (oinfo (parse-library-file f))
       (root (resource-file "goldfish/hint"))
       (closure (source-closure oinfo (list root)))
       (libs (exporting-libs 'dep-fn closure))
      ) ;
  (check-true (list? libs))
  (check (> (length libs) 0) => #t)
) ;let*

(check-report)
