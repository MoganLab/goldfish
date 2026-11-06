;; find-visible-library-root
;; 根据库查询字符串查找当前加载路径中可见的源码根目录。
;;
;; 语法
;; ----
;; (find-visible-library-root query)
;;
;; 参数
;; ----
;; query : string
;;   形如 "liii/string" 的库查询字符串。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回包含该库的加载根目录；库不可见或查询无效时返回 #f。

(set! *load-path* (cons "tools/doc" *load-path*))

(import (liii check) (liii golddoc library) (liii path))

(check-set-mode! 'report-failed)

(let ((root (find-visible-library-root "liii/string")))
  (check-true (string? root))
  (check-true (path-file? (path-join root "liii" "string.scm")))
) ;let

(let ((root (find-visible-library-root "liii/uri/compare")))
  (check-true (string? root))
  (check-true (path-file? (path-join root "liii" "uri" "compare.scm")))
) ;let

(check (find-visible-library-root "liii/not-a-real-library") => #f)
(check (find-visible-library-root "invalid") => #f)

(check-report)
