;; parse-library-structure
;; 解析 Scheme 库源码文件的完整声明结构，提取导出表、导入表和本地定义。
;;
;; 语法
;; ----
;; (parse-library-structure source-path)
;;
;; 参数
;; ----
;; source-path : string
;; Scheme 库源码实现文件路径。
;;
;; 返回值
;; ------
;; list or #f
;; 若成功解析出 define-library，则返回包含 exports、imports 和 defs 的结构化 alist，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii path)
  (liii goldcheck unit-test source parse)
) ;import

(check-set-mode! 'report-failed)

;; 1. 不存在的文件返回 #f
(check (parse-library-structure "non-existent-file.scm") => #f)

;; 2. 解析标准的 define-library 文件结构
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-test-parse-struct.scm"))
       ) ;tmp-file
       (content (string-append "(define-library (test foo)\n"
                  "  (export bar (rename baz qux))\n"
                  "  (import (scheme base))\n"
                  "  (begin (define (bar) 1) (define baz 2)))\n"
                ) ;string-append
       ) ;content
      ) ;
  (path-write-bytes (path tmp-file) (string->utf8 content))
  (let ((struct (parse-library-structure tmp-file)))
    (check-true (pair? struct))
    (check (cdr (assq 'exports struct)) => '(("bar" . "bar") ("qux" . "baz")))
    (check (cdr (assq 'imports struct)) => '((scheme base)))
    (check (cdr (assq 'defs struct)) => '("bar" "baz"))
  ) ;let
  (path-unlink tmp-file #t)
) ;let*

(check-report)
