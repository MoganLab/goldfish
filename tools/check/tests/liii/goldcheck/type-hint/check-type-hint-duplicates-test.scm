;; check-type-hint-duplicates
;; 检查跨 hint 库之间是否存在同名签名的重复声明。
;;
;; 语法
;; ----
;; (check-type-hint-duplicates hint-file-list)
;;
;; 参数
;; ----
;; hint-file-list : list of string  待检测的 hint 库文件路径列表
;;
;; 返回值
;; ------
;; list of violation
;; 跨库重复定义的违规列表，无重复返回空列表 '()。

(import (scheme base) (liii check) (liii os) (liii goldcheck type-hint))

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

;; 2. 同一库内多次声明不计入跨库重复
(check (check-type-hint-duplicates (list (resource-file "goldfish/hint/liii/a7dupdef.scm"))
       ) ;check-type-hint-duplicates
  =>
  '()
) ;check

(check-report)
