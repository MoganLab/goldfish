;; check-a3
;; 校验 hint 库是否导入了其他 hint 库（A3）。
;;
;; 语法
;; ----
;; (check-a3 rel info)
;;
;; 参数
;; ----
;; rel : string  相对路径
;; info : list   hint 库解析 AST
;;
;; 返回值
;; ------
;; list of violation
;; 违规列表；未导入其他 hint 库返回空列表 '()。

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

;; 1. 未导入 hint 库无违规
(let* ((f (resource-file "goldfish/hint/liii/clean.scm"))
       (info (parse-library-file f))
       (viols (check-a3 f info))
      ) ;
  (check viols => '())
) ;let*

;; 2. 导入了 hint 库产生违规
(let* ((f (resource-file "goldfish/hint/liii/importhint.scm"))
       (info (parse-library-file f))
       (viols (check-a3 f info))
      ) ;
  (check (length viols) => 1)
) ;let*

(check-report)
