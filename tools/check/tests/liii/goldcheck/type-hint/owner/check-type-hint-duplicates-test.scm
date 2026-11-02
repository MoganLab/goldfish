;; check-type-hint-duplicates
;; 校验跨 hint 库之间的签名重复声明（A7c）。
;;
;; 语法
;; ----
;; (check-type-hint-duplicates hint-files)
;;
;; 参数
;; ----
;; hint-files : list of string  hint 库文件路径列表
;;
;; 返回值
;; ------
;; list of violation
;; 跨库重复定义的违规列表；无重复时返回空列表 '()。

(import (scheme base)
  (liii check)
  (liii string)
  (liii os)
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

;; 1. 空列表无违规
(check (check-type-hint-duplicates '()) => '())

;; 2. 两个互相重复声明相同函数的库报错
(let* ((f1 (resource-file "goldfish/hint/liii/a7dupx.scm"))
       (f2 (resource-file "goldfish/hint/liii/a7dupy.scm"))
       (viols (check-type-hint-duplicates (list f1 f2)))
      ) ;
  (check (length viols) => 2)
  (check-true (string-contains? (cadddr (car viols)) "dup-fn"))
) ;let*

(check-report)
