;; check-infer
;; 对指定 hint 库文件及其原库执行静态类型推导与实参类型冲突检查。
;;
;; 语法
;; ----
;; (check-infer hint-file)
;;
;; 参数
;; ----
;; hint-file : string  类型提示库文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 检测到的类型冲突违规列表；无冲突返回空列表 '()。

(import (scheme base)
  (liii check)
  (liii string)
  (liii os)
  (liii goldcheck type-hint infer)
) ;import

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 1. 不存在原库的 hint 文件跳过返回空列表
(check (check-infer "non-existent-hint.scm") => '())

;; 2. 检查内部函数冲突报告
(let ((f (resource-file "goldfish/hint/liii/inferok.scm")))
  (when (access f 'R_OK)
    (let ((viols (check-infer f)))
      (check (length viols) => 1)
      (check-true (string-contains? (cadddr (car viols)) "my-cdr"))
    ) ;let
  ) ;when
) ;let

(check-report)
