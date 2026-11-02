;; hint-lib-file
;; 获取指定库符号在给定 hint 根目录列表下对应的 hint 库文件路径。
;;
;; 语法
;; ----
;; (hint-lib-file lib-name roots)
;;
;; 参数
;; ----
;; lib-name : list of symbol  库名符号列表（如 '(liii clean)）
;; roots : list of string     hint 根目录列表
;;
;; 返回值
;; ------
;; string 或 #f
;; hint 库文件路径；未找到返回 #f。

(import (scheme base) (liii check) (liii os) (liii goldcheck type-hint env))

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 1. 查找存在的 hint 库文件
(let* ((root (resource-file "goldfish/hint"))
       (file (hint-lib-file '(liii clean) (list root)))
      ) ;
  (check-true (string? file))
) ;let*

;; 2. 不存在时返回 #f
(check (hint-lib-file '(non-existent) '()) => #f)

(check-report)
