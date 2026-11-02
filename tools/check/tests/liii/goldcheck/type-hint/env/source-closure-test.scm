;; source-closure
;; 按广度优先顺序返回原库及其全部依赖项的 import 闭包 ((深度 . 库信息) ...)。
;;
;; 语法
;; ----
;; (source-closure oinfo roots)
;;
;; 参数
;; ----
;; oinfo : list  原库解析 AST
;; roots : list  hint 查找根目录列表
;;
;; 返回值
;; ------
;; list of pair
;; 依赖闭包列表，每项为 (深度 . 库解析信息)。

(import (scheme base)
  (liii check)
  (liii os)
  (liii goldcheck type-hint parse)
  (liii goldcheck type-hint env)
) ;import

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 1. 获取依赖闭包
(let* ((f (resource-file "goldfish/liii/a7reexport.scm"))
       (oinfo (parse-library-file f))
       (root (resource-file "goldfish/hint"))
       (closure (source-closure oinfo (list root)))
      ) ;
  (check-true (list? closure))
  (check (> (length closure) 0) => #t)
  ;; 第 0 项为自身
  (check (caar closure) => 0)
) ;let*

(check-report)
