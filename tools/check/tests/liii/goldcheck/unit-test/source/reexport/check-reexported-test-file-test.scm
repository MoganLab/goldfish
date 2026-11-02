;; check-reexported-test-file
;; 检查单个单元测试文件是否属于非本库实现的重导出函数。
;;
;; 语法
;; ----
;; (check-reexported-test-file file-path)
;;
;; 参数
;; ----
;; file-path : string
;; 单元测试文件路径。
;;
;; 返回值
;; ------
;; list
;; 若为重导出函数测试则返回包含违规详情的列表，合规或非测试文件时返回 '()。

(import (scheme base)
  (liii check)
  (liii path)
  (liii string)
  (liii goldcheck unit-test source reexport)
) ;import

(check-set-mode! 'report-failed)

;; 1. 非 -test.scm 结尾文件返回空列表
(check (check-reexported-test-file "README.md") => '())
(check (check-reexported-test-file "source.scm") => '())

;; 2. 未设置源码查找函数或找不到对应库源码时返回空列表
(check (check-reexported-test-file "tests/non-existent/foo-test.scm") => '())

;; 3. 夹具测试：检查重导出函数测试文件
(let* ((tmp-dir (path-temp-dir))
       (base-dir (path-join tmp-dir "gf-test-reexport-file"))
       (goldfish-dir (path-join base-dir "goldfish"))
       (test-dir (path-join base-dir "tests" "my-lib"))
       (_ (path-mkdir (path goldfish-dir) :parents #t :exist-ok #t))
       (_ (path-mkdir (path test-dir) :parents #t :exist-ok #t))
       (src-mod (path->string (path-join goldfish-dir "src-mod.scm")))
       (wrap-mod (path->string (path-join goldfish-dir "wrap-mod.scm")))
       (wrap-test (path->string (path-join test-dir "foreign-fn-test.scm")))
       (src-code (string-append "(define-library (src-mod)\n"
                   "  (export foreign-fn)\n" "  (import (scheme base))\n"
                   "  (begin (define (foreign-fn) 1)))\n"
                 ) ;string-append
       ) ;src-code
       (wrap-code (string-append "(define-library (wrap-mod)\n"
                    "  (export foreign-fn local-fn)\n"
                    "  (import (scheme base) (src-mod))\n"
                    "  (begin (define (local-fn) 2)))\n"
                  ) ;string-append
       ) ;wrap-code
      ) ;
  (path-write-bytes (path src-mod) (string->utf8 src-code))
  (path-write-bytes (path wrap-mod) (string->utf8 wrap-code))
  (path-write-bytes (path wrap-test) (string->utf8 "(check 1 => 1)\n"))
  (set-find-library-source-fn! (lambda (p) wrap-mod))
  (let ((viols (check-reexported-test-file wrap-test)))
    (check-true (pair? viols))
    (check-true (string-contains? (list-ref (car viols) 3) "并非本库实现"))
  ) ;let
  (set-find-library-source-fn! #f)

  (path-unlink wrap-test #t)
  (path-unlink wrap-mod #t)
  (path-unlink src-mod #t)
  (path-rmdir test-dir)
  (path-rmdir (path->string (path-parent (path test-dir))))
  (path-rmdir goldfish-dir)
  (path-rmdir base-dir)
) ;let*

(check-report)
