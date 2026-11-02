;; library-file
;; 在指定根目录列表中定位库的源文件或 hint 文件路径。
;;
;; 语法
;; ----
;; (library-file roots lib-syms source?)
;;
;; 参数
;; ----
;; roots : list of string     查找根目录列表
;; lib-syms : list of symbol  库名符号列表（如 '(liii clean)）
;; source? : boolean          #t 查找原库源码，#f 查找 hint 库文件
;;
;; 返回值
;; ------
;; string 或 #f
;; 命中文件的路径字符串；未找到时返回 #f。

(import (scheme base) (liii check) (liii os) (liii goldcheck type-hint env))

(check-set-mode! 'report-failed)

(define (resource-file rel)
  (let ((local (string-append "tests/resources/type-hint/" rel))
        (alt (string-append "tools/check/tests/resources/type-hint/" rel))
       ) ;
    (if (access local 'R_OK) local alt)
  ) ;let
) ;define

;; 1. 查找存在的原库文件
(let* ((root (resource-file "goldfish/hint"))
       (file (library-file (list root) '(liii clean) #t))
      ) ;
  (check-true (string? file))
) ;let*

;; 2. 根列表为空或库不存在返回 #f
(check (library-file '() '(liii string) #t) => #f)
(check (library-file '("non-existent-root") '(non-existent-lib) #t) => #f)

(check-report)
