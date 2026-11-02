;; check-var
;; 校验单个 hint 变量定义的签名语法结构与重复签名（A4/A6）。
;;
;; 语法
;; ----
;; (check-var rel d)
;;
;; 参数
;; ----
;; rel : string  相对路径
;; d : def       单个 hint 变量的 def 定义记录
;;
;; 返回值
;; ------
;; list of violation
;; 违规列表；签名语法合法且无重复时返回空列表 '()。

(import (scheme base)
  (liii check)
  (liii os)
  (liii goldcheck type-hint parse)
  (liii goldcheck type-hint hint-data)
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

;; 1. 合法签名定义无违规
(let* ((f (resource-file "goldfish/hint/liii/clean.scm"))
       (info (parse-library-file f))
       (defs (hint-var-defs info))
       (d (car defs))
       (viols (check-var f d))
      ) ;
  (check viols => '())
) ;let*

;; 2. 错误签名定义产生违规
(let* ((f (resource-file "goldfish/hint/liii/badsig.scm"))
       (info (parse-library-file f))
       (defs (hint-var-defs info))
       (d (car defs))
       (viols (check-var f d))
      ) ;
  (check (length viols) => 1)
) ;let*

(check-report)
