;; check-unit-test-dirs
;; 检查所有测试文件所属的目录规范：包含 README.md、无废弃单测试文件、导出函数均有对应单测。
;;
;; 语法
;; ----
;; (check-unit-test-dirs files)
;;
;; 参数
;; ----
;; files : list of string
;; 待检查的测试文件路径列表。
;;
;; 返回值
;; ----
;; list
;; 违规项列表。无违规时返回 '()。

(import (scheme base)
  (liii check)
  (liii list)
  (liii path)
  (liii string)
  (liii goldcheck unit-test)
) ;import

(check-set-mode! 'report-failed)

;; 1. 违规用例：测试目录下缺少库导览文档 README.md
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "gf-check-test-dir-no-readme"))
       ) ;tmp-dir
       (_ (path-mkdir (path tmp-dir) :parents #t :exist-ok #t))
       (test-file (path->string (path-join tmp-dir "foo-test.scm")))
       (_ (path-write-bytes (path test-file) (string->utf8 "(check 1 => 1)\n")))
       (viols (check-unit-test-dirs (list test-file)))
      ) ;
  (check-true (not (null? viols)))
  (check (car (car viols)) => 'unit-test)
  (check-true (string-contains? (list-ref (car viols) 3) "缺少库导览文档 README.md")
  ) ;check-true
  (path-unlink test-file #t)
  (path-rmdir tmp-dir)
) ;let*

;; 2. 合规用例：测试目录下包含 README.md
(let* ((tmp-dir (path->string (path-join (path-temp-dir) "gf-check-test-dir-has-readme"))
       ) ;tmp-dir
       (_ (path-mkdir (path tmp-dir) :parents #t :exist-ok #t))
       (test-file (path->string (path-join tmp-dir "foo-test.scm")))
       (readme-file (path->string (path-join tmp-dir "README.md")))
       (_ (path-write-bytes (path test-file) (string->utf8 "(check 1 => 1)\n")))
       (_ (path-write-bytes (path readme-file) (string->utf8 "# Readme\n")))
       (viols (check-unit-test-dirs (list test-file)))
      ) ;
  (check viols => '())
  (path-unlink test-file #t)
  (path-unlink readme-file #t)
  (path-rmdir tmp-dir)
) ;let*

;; 3. 违规用例：已拆分测试目录但仍保留同名测试文件（废弃方案）
(let* ((base-dir (path->string (path-join (path-temp-dir) "gf-check-test-legacy")))
       (sub-dir (path->string (path-join base-dir "foo")))
       (_ (path-mkdir (path sub-dir) :parents #t :exist-ok #t))
       (sub-test (path->string (path-join sub-dir "bar-test.scm")))
       (readme (path->string (path-join sub-dir "README.md")))
       (legacy-file (path->string (path-join base-dir "foo-test.scm")))
       (_ (path-write-bytes (path sub-test) (string->utf8 "(check 1 => 1)\n")))
       (_ (path-write-bytes (path readme) (string->utf8 "# Readme\n")))
       (_ (path-write-bytes (path legacy-file) (string->utf8 ";; legacy\n")))
       (viols (check-unit-test-dirs (list sub-test)))
      ) ;
  (check-true (not (null? viols)))
  (check (car (car viols)) => 'unit-test)
  (check-true (string-contains? (list-ref (car viols) 3) "这个方案已经废弃")
  ) ;check-true
  (check-true (string-contains? (list-ref (car viols) 3) "README.md"))
  (path-unlink legacy-file #t)
  (path-unlink sub-test #t)
  (path-unlink readme #t)
  (path-rmdir sub-dir)
  (path-rmdir base-dir)
) ;let*

;; 4. 违规用例：测试目录下缺少导出函数的单元测试
(let* ((base-dir (path->string (path-join (path-temp-dir) "gf-check-test-missing-fn")))
       (test-dir (path->string (path-join base-dir "tests" "foo")))
       (src-dir (path->string (path-join base-dir "goldfish")))
       (_ (path-mkdir (path test-dir) :parents #t :exist-ok #t))
       (_ (path-mkdir (path src-dir) :parents #t :exist-ok #t))
       (src-file (path->string (path-join src-dir "foo.scm")))
       (readme (path->string (path-join test-dir "README.md")))
       (bar-test (path->string (path-join test-dir "bar-test.scm")))
       (src-content "(define-library (foo)\n  (export bar baz)\n  (import (scheme base))\n  (begin (define bar 1) (define baz 2)))\n"
       ) ;src-content
       (_ (path-write-bytes (path src-file) (string->utf8 src-content)))
       (_ (path-write-bytes (path readme) (string->utf8 "# foo\n")))
       (_ (path-write-bytes (path bar-test) (string->utf8 "(check 1 => 1)\n")))
       (viols (check-unit-test-dirs (list bar-test)))
      ) ;
  (check-true (not (null? viols)))
  (check (car (car viols)) => 'unit-test)
  (check-true (string-contains? (list-ref (car viols) 3)
                "缺少导出函数 baz 的单元测试"
              ) ;string-contains?
  ) ;check-true
  (path-unlink bar-test #t)
  (path-unlink readme #t)
  (path-unlink src-file #t)
  (path-rmdir test-dir)
  (path-rmdir (path->string (path-parent (path test-dir))))
  (path-rmdir src-dir)
  (path-rmdir base-dir)
) ;let*

;; 5. 合规用例：测试目录下包含所有导出函数的单元测试
(let* ((base-dir (path->string (path-join (path-temp-dir) "gf-check-test-all-fn")))
       (test-dir (path->string (path-join base-dir "tests" "foo")))
       (src-dir (path->string (path-join base-dir "goldfish")))
       (_ (path-mkdir (path test-dir) :parents #t :exist-ok #t))
       (_ (path-mkdir (path src-dir) :parents #t :exist-ok #t))
       (src-file (path->string (path-join src-dir "foo.scm")))
       (readme (path->string (path-join test-dir "README.md")))
       (bar-test (path->string (path-join test-dir "bar-test.scm")))
       (baz-test (path->string (path-join test-dir "baz-test.scm")))
       (src-content "(define-library (foo)\n  (export bar baz)\n  (import (scheme base))\n  (begin (define bar 1) (define baz 2)))\n"
       ) ;src-content
       (_ (path-write-bytes (path src-file) (string->utf8 src-content)))
       (_ (path-write-bytes (path readme) (string->utf8 "# foo\n")))
       (_ (path-write-bytes (path bar-test) (string->utf8 "(check 1 => 1)\n")))
       (_ (path-write-bytes (path baz-test) (string->utf8 "(check 1 => 1)\n")))
       (viols (check-unit-test-dirs (list bar-test baz-test)))
      ) ;
  (check viols => '())
  (path-unlink bar-test #t)
  (path-unlink baz-test #t)
  (path-unlink readme #t)
  (path-unlink src-file #t)
  (path-rmdir test-dir)
  (path-rmdir (path->string (path-parent (path test-dir))))
  (path-rmdir src-dir)
  (path-rmdir base-dir)
) ;let*

;; 6. 传入源码文件时自动推断对应测试目录，并检查缺失导出函数的单元测试
(let* ((base-dir (path->string (path-join (path-temp-dir) "gf-check-test-src-missing-fn"))
       ) ;base-dir
       (test-dir (path->string (path-join base-dir "tests" "foo")))
       (src-dir (path->string (path-join base-dir "goldfish")))
       (_ (path-mkdir (path test-dir) :parents #t :exist-ok #t))
       (_ (path-mkdir (path src-dir) :parents #t :exist-ok #t))
       (src-file (path->string (path-join src-dir "foo.scm")))
       (readme (path->string (path-join test-dir "README.md")))
       (bar-test (path->string (path-join test-dir "bar-test.scm")))
       (src-content "(define-library (foo)\n  (export bar baz)\n  (import (scheme base))\n  (begin (define bar 1) (define baz 2)))\n"
       ) ;src-content
       (_ (path-write-bytes (path src-file) (string->utf8 src-content)))
       (_ (path-write-bytes (path readme) (string->utf8 "# foo\n")))
       (_ (path-write-bytes (path bar-test)
            (string->utf8 "(check 1 => 1)\n\n(check-report)\n")
          ) ;path-write-bytes
       ) ;_
       (viols (check-unit-test-dirs (list src-file)))
      ) ;
  (check-true (not (null? viols)))
  (check (car (car viols)) => 'unit-test)
  (check-true (string-contains? (list-ref (car viols) 3)
                "缺少导出函数 baz 的单元测试"
              ) ;string-contains?
  ) ;check-true
  (path-unlink bar-test #t)
  (path-unlink readme #t)
  (path-unlink src-file #t)
  (path-rmdir test-dir)
  (path-rmdir (path->string (path-parent (path test-dir))))
  (path-rmdir src-dir)
  (path-rmdir base-dir)
) ;let*

;; 7. 传入源码文件时，自动检查对应测试目录下测试文件的内容规范
(let* ((base-dir (path->string (path-join (path-temp-dir) "gf-check-test-src-file-content"))
       ) ;base-dir
       (test-dir (path->string (path-join base-dir "tests" "foo")))
       (src-dir (path->string (path-join base-dir "goldfish")))
       (_ (path-mkdir (path test-dir) :parents #t :exist-ok #t))
       (_ (path-mkdir (path src-dir) :parents #t :exist-ok #t))
       (src-file (path->string (path-join src-dir "foo.scm")))
       (readme (path->string (path-join test-dir "README.md")))
       (bar-test (path->string (path-join test-dir "bar-test.scm")))
       (src-content "(define-library (foo)\n  (export bar)\n  (import (scheme base))\n  (begin (define bar 1)))\n"
       ) ;src-content
       (bad-test-content "(import (scheme base) (liii check))\n(check 1 => 1)\n")
       (_ (path-write-bytes (path src-file) (string->utf8 src-content)))
       (_ (path-write-bytes (path readme) (string->utf8 "# foo\n")))
       (_ (path-write-bytes (path bar-test) (string->utf8 bad-test-content)))
       (viols (check-unit-test-dirs (list src-file)))
      ) ;
  (check-true (not (null? viols)))
  (let ((found
          (filter (lambda (v) (string-contains? (list-ref v 1) "bar-test.scm")) viols)
        ) ;found
       ) ;
    (check-true (not (null? found)))
  ) ;let
  (path-unlink bar-test #t)
  (path-unlink readme #t)
  (path-unlink src-file #t)
  (path-rmdir test-dir)
  (path-rmdir (path->string (path-parent (path test-dir))))
  (path-rmdir src-dir)
  (path-rmdir base-dir)
) ;let*

;; 8. 传入非库源码文件（无 define-library）时自动过滤
(let* ((base-dir (path->string (path-join (path-temp-dir) "gf-check-test-non-lib")))
       (src-dir (path->string (path-join base-dir "goldfish")))
       (_ (path-mkdir (path src-dir) :parents #t :exist-ok #t))
       (script-file (path->string (path-join src-dir "script.scm")))
       (_ (path-write-bytes (path script-file) (string->utf8 "(display \"hello\")\n")))
       (viols (check-unit-test-dirs (list script-file)))
      ) ;
  (check viols => '())
  (path-unlink script-file #t)
  (path-rmdir src-dir)
  (path-rmdir base-dir)
) ;let*

(check-report)
