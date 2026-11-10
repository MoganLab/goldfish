;; find-tests-root-for-load-root
;; 根据源码加载根目录定位对应的测试根目录。
;;
;; 语法
;; ----
;; (find-tests-root-for-load-root load-root)
;;
;; 参数
;; ----
;; load-root : string
;;   库源码的加载根目录。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   优先返回加载根目录内的 tests，其次返回其兄弟 tests 目录；
;;   两者均不存在时返回 #f。

(set! *load-path* (cons "tools/doc" *load-path*))

(import (liii check) (liii golddoc library) (liii path))

(check-set-mode! 'report-failed)

(let* ((load-root (find-visible-library-root "liii/string"))
       (tests-root (find-tests-root-for-load-root load-root))
      ) ;
  (check-true (string? tests-root))
  (check-true (path-dir? tests-root))
  (check-true (path-file? (path-join tests-root "liii" "string" "README.md")))
) ;let*

(let* ((load-root (find-visible-library-root "liii/uri/compare"))
       (tests-root (find-tests-root-for-load-root load-root))
      ) ;
  (check-true (string? tests-root))
  (check-true (path-file? (path-join tests-root "liii" "uri" "compare" "README.md"))
  ) ;check-true
) ;let*

(check (find-tests-root-for-load-root #f) => #f)
(check (find-tests-root-for-load-root "/not/a/real/load/root") => #f)

(check-report)
