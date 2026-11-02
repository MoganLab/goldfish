;; check-a7
;; 校验 hint 库内签名归属（A7a）与库内重复声明（A7b）。
;;
;; 语法
;; ----
;; (check-a7 rel info oinfo roots)
;;
;; 参数
;; ----
;; rel : string    hint 库相对路径
;; info : list     hint 库解析数据结构
;; oinfo : list    原库解析数据结构或 #f
;; roots : list    hint 查找根目录列表
;;
;; 返回值
;; ------
;; list of violation
;; 违规列表；合规时返回空列表 '()。

(import (scheme base)
  (liii check)
  (liii string)
  (liii os)
  (liii goldcheck type-hint parse)
  (liii goldcheck type-hint env)
  (liii goldcheck type-hint owner)
) ;import

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 1. 库内重复变量定义检测 (A7b)
(let* ((f (resource-file "goldfish/hint/liii/a7dupdef.scm"))
       (info (parse-library-file f))
       (viols (check-a7 f info #f '()))
      ) ;
  (check-true (> (length viols) 0))
  (check-true (string-contains? (cadddr (car viols)) "duplicate definition"))
) ;let*

(check-report)
