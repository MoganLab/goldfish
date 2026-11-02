;; read-all-forms
;; 读取指定 Scheme 源文件中所有的顶级 S 表达式形式列表。
;;
;; 语法
;; ----
;; (read-all-forms path)
;;
;; 参数
;; ----
;; path : string  源文件路径
;;
;; 返回值
;; ------
;; list
;; 读取到的所有顶级 S 表达式形式列表。

(import (scheme base) (liii check) (liii os) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

(let* ((f (resource-file "script.scm")) (forms (read-all-forms f)))
  (check-true (list? forms))
  (check-true (> (length forms) 0))
) ;let*

(check-report)
