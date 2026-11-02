;; full-coverage?
;; 判断目标原库导出的所有符号是否均在给定的类型环境中拥有签名。
;;
;; 语法
;; ----
;; (full-coverage? env oinfo)
;;
;; 参数
;; ----
;; env : list    类型环境绑定列表 ((name . sigs) ...)
;; oinfo : list  原库解析 AST
;;
;; 返回值
;; ------
;; boolean
;; 全部导出符号均有类型签名时返回 #t，存在未覆盖的导出时返回 #f。

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

;; 1. 签名全覆盖返回 #t
(let* ((orig (resource-file "goldfish/liii/inferok.scm"))
       (hint (resource-file "goldfish/hint/liii/inferok.scm"))
       (oinfo (parse-library-file orig))
       (env (build-env oinfo hint))
      ) ;
  (check (full-coverage? env oinfo) => #t)
) ;let*

;; 2. 部分未覆盖返回 #f
(let* ((orig (resource-file "goldfish/liii/partial.scm"))
       (hint (resource-file "goldfish/hint/liii/partial.scm"))
       (oinfo (parse-library-file orig))
       (env (build-env oinfo hint))
      ) ;
  (check (full-coverage? env oinfo) => #f)
) ;let*

(check-report)
