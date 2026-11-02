;; check-a2
;; 校验 hint 库是否导出了约定命名的标准查询函数（A2）。
;;
;; 语法
;; ----
;; (check-a2 rel info qname)
;;
;; 参数
;; ----
;; rel : string    相对路径
;; info : list     hint 库解析 AST
;; qname : symbol  标准查询函数符号（如 'hint-liii/clean）
;;
;; 返回值
;; ------
;; list of violation
;; 违规列表；已导出时返回空列表 '()。

(import (scheme base)
  (liii check)
  (liii os)
  (liii goldcheck type-hint parse)
  (liii goldcheck type-hint checks)
) ;import

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 1. 已导出约定查询函数
(let* ((f (resource-file "goldfish/hint/liii/clean.scm"))
       (info (parse-library-file f))
       (viols (check-a2 f info 'hint-liii/clean))
      ) ;
  (check viols => '())
) ;let*

;; 2. 未导出约定查询函数
(let* ((f (resource-file "goldfish/hint/liii/clean.scm"))
       (info (parse-library-file f))
       (viols (check-a2 f info 'not-exported-func))
      ) ;
  (check (length viols) => 1)
) ;let*

(check-report)
