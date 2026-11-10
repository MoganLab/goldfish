;; find-defining-library-test-dir
;; 查找重导出符号在其被导入的原始定义库中对应的测试目录。
;;
;; 语法
;; ----
;; (find-defining-library-test-dir int-name source-path)
;;
;; 参数
;; ----
;; int-name : string
;; 重导出符号的内部名称。
;; source-path : string
;; 当前库的源码文件路径。
;;
;; 返回值
;; ------
;; string or #f
;; 符号定义库对应的测试目录路径（如 "goldfish/tests/foo"）；未找到时返回 #f。

(import (scheme base)
  (liii check)
  (liii path)
  (liii goldcheck unit-test source reexport)
) ;import

(check-set-mode! 'report-failed)

;; 1. 不存在的文件返回 #f
(check (find-defining-library-test-dir "foo" "non-existent.scm") => #f)

;; 2. 夹具测试：在导入库中成功查找符号定义库的测试目录
(let* ((tmp-dir (path-temp-dir))
       (base-dir (path-join tmp-dir "gf-test-def-lib"))
       (goldfish-dir (path-join base-dir "goldfish"))
       (foo-file (path->string (path-join goldfish-dir "foo.scm")))
       (bar-file (path->string (path-join goldfish-dir "bar.scm")))
       (_ (path-mkdir (path goldfish-dir) :parents #t :exist-ok #t))
       (foo-src (string-append "(define-library (foo)\n"
                  "  (export helper)\n"
                  "  (import (scheme base))\n"
                  "  (begin (define (helper) 1)))\n"
                ) ;string-append
       ) ;foo-src
       (bar-src (string-append "(define-library (bar)\n"
                  "  (export helper)\n"
                  "  (import (scheme base) (foo))\n"
                  "  (begin))\n"
                ) ;string-append
       ) ;bar-src
      ) ;
  (path-write-bytes (path foo-file) (string->utf8 foo-src))
  (path-write-bytes (path bar-file) (string->utf8 bar-src))
  (check (find-defining-library-test-dir "helper" bar-file) => "goldfish/tests/foo")
  (check (find-defining-library-test-dir "unknown-sym" bar-file) => #f)

  (path-unlink foo-file #t)
  (path-unlink bar-file #t)
  (path-rmdir goldfish-dir)
  (path-rmdir base-dir)
) ;let*

(check-report)
