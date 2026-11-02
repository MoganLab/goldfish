;; check-b
;; 逐函数校验 hint 签名参数数量是否与原库函数定义的 arity 一致（B2）。
;;
;; 语法
;; ----
;; (check-b rel hint-file entries vars)
;;
;; 参数
;; ----
;; rel : string        相对路径
;; hint-file : string  hint 库完整路径
;; entries : list      查询函数登记条目
;; vars : list         hint 变量 def 列表
;;
;; 返回值
;; ------
;; list of violation
;; 违规列表；参数数量全部一致时返回空列表 '()。

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

;; 1. 原库参数数量一致时无违规
(let* ((f (resource-file "goldfish/hint/liii/clean.scm"))
       (info (parse-library-file f))
       (vars (hint-var-defs info))
       (entries (registered-entries info 'hint-liii/clean))
       (viols (check-b f f entries vars))
      ) ;
  (check viols => '())
) ;let*

;; 2. 原库参数数量不一致时产生违规
(let* ((f (resource-file "goldfish/hint/liii/arity.scm"))
       (info (parse-library-file f))
       (vars (hint-var-defs info))
       (entries (registered-entries info 'hint-liii/arity))
       (viols (check-b f f entries vars))
      ) ;
  (check (length viols) => 2)
) ;let*

(check-report)
