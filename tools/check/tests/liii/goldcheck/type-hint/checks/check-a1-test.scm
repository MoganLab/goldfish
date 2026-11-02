;; check-a1
;; 校验 hint 库名与文件路径是否对应（A1）。
;;
;; 语法
;; ----
;; (check-a1 rel hint-file info)
;;
;; 参数
;; ----
;; rel : string        相对路径
;; hint-file : string  hint 库完整路径
;; info : list         hint 库解析 AST
;;
;; 返回值
;; ------
;; list of violation
;; 违规列表；库名与路径一致返回空列表 '()。

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

;; 1. 库名与路径匹配时无违规
(let* ((f (resource-file "goldfish/hint/liii/clean.scm"))
       (info (parse-library-file f))
       (viols (check-a1 f f info))
      ) ;
  (check viols => '())
) ;let*

;; 2. 库名与路径不匹配时产生违规
(let* ((f (resource-file "goldfish/hint/liii/wrongname.scm"))
       (info (parse-library-file f))
       (viols (check-a1 f f info))
      ) ;
  (check (length viols) => 1)
) ;let*

(check-report)
