;; parse-library-file
;; 解析指定路径的 define-library 源文件，提取结构数据 alist。
;;
;; 语法
;; ----
;; (parse-library-file path)
;;
;; 参数
;; ----
;; path : string  库文件路径
;;
;; 返回值
;; ------
;; list 或 #f
;; 库解析数据结构 alist；非 define-library 文件返回 #f。

(import (scheme base) (liii check) (liii os) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 1. 解析有效的库文件
(let* ((f (resource-file "goldfish/hint/liii/clean.scm"))
       (info (parse-library-file f))
      ) ;
  (check-true (list? info))
  (check (cdr (assq 'lib-name info)) => '(hint liii clean))
) ;let*

;; 2. 非库文件返回 #f
(let* ((f (resource-file "script.scm")) (info (parse-library-file f)))
  (check info => #f)
) ;let*

(check-report)
