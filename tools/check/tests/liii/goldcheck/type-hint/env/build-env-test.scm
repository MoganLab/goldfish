;; build-env
;; 为目标原库与 hint 库构建包含自身签名、import 依赖穿透及内建基础签名的类型环境。
;;
;; 语法
;; ----
;; (build-env oinfo hint-file)
;;
;; 参数
;; ----
;; oinfo : list        原库解析 AST
;; hint-file : string  hint 库文件路径
;;
;; 返回值
;; ------
;; list of pair
;; 类型环境绑定列表 ((符号名 . 签名列表) ...)。

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

(let* ((orig (resource-file "goldfish/liii/clean.scm"))
       (hint (resource-file "goldfish/hint/liii/clean.scm"))
       (oinfo (parse-library-file orig))
       (env (build-env oinfo hint))
      ) ;
  (check-true (list? env))
  (check (> (length env) 0) => #t)
  (check-true (and (assq 'f1 env) #t))
) ;let*

(check-report)
