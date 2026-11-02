;; cached-library-info
;; 获取指定路径库文件的 AST 解析信息（带内存缓存避免重复读取）。
;;
;; 语法
;; ----
;; (cached-library-info path)
;;
;; 参数
;; ----
;; path : string 或 #f  库文件路径
;;
;; 返回值
;; ------
;; list 或 #f
;; 库解析数据结构；path 为 #f 或文件不存在时返回 #f。

(import (scheme base) (liii check) (liii os) (liii goldcheck type-hint env))

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 1. path 为 #f 返回 #f
(check (cached-library-info #f) => #f)

;; 2. 读取并缓存库信息
(let* ((f (resource-file "goldfish/hint/liii/clean.scm"))
       (info1 (cached-library-info f))
       (info2 (cached-library-info f))
      ) ;
  (check-true (list? info1))
  (check (eq? info1 info2) => #t)
) ;let*

(check-report)
