;; normalize-import-mapping
;; 将 only、except、prefix、rename 导入形式规范为绑定映射。
;;
;; 语法
;; ----
;; (normalize-import-mapping clause . roots)
;;
;; 参数
;; ----
;; clause : list  导入子句
;; roots : list  可选源码根列表
;;
;; 返回值
;; -------
;; list
;; (bound-name . exported-name) 映射或 indeterminate。

(import (scheme base) (liii check) (liii goldcheck analysis))

(check-set-mode! 'report-failed)

(let ((m (normalize-import-mapping '(only (analysis first) local-name)
           (list "tests/resources")
         ) ;normalize-import-mapping
      ) ;m
     ) ;
  (check m => '((local-name . local-name)))
) ;let

(let ((m (normalize-import-mapping '(except (analysis first) local-name)
           (list "tests/resources")
         ) ;normalize-import-mapping
      ) ;m
     ) ;
  (check-true (not (assq 'local-name m)))
) ;let

(let ((m (normalize-import-mapping '(prefix (analysis first) pre-)
           (list "tests/resources")
         ) ;normalize-import-mapping
      ) ;m
     ) ;
  (check-true (pair? (assq 'pre-local-name m)))
) ;let

(let ((m (normalize-import-mapping '(rename (analysis first)
                                      (local-name renamed-name))
           (list "tests/resources")
         ) ;normalize-import-mapping
      ) ;m
     ) ;
  (check-true (pair? (assq 'renamed-name m)))
) ;let

(check-report)
