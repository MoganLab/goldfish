;; check-reexported-tests
;; 检查测试目录下是否存在本库未实现的重导出函数的单元测试，若存在则提示应移动到的目录。
;;
;; 语法
;; ----
;; (check-reexported-tests parent rel-parent)
;;
;; 参数
;; ----
;; parent : string
;; 单元测试目录的完整或相对路径。
;; rel-parent : string
;; 单元测试目录相对于工程根的相对路径。
;;
;; 返回值
;; ------
;; list
;; 违规项列表。无违规时返回空列表 '()。
;;
;; 说明
;; ----
;; 若一个函数并非当前库本地实现（仅是从其他库 import 并 re-export），
;; 则当前测试目录下不应为其提供单测，应指出该单测需移动到真正实现的库对应的测试目录。

(import (scheme base)
  (liii check)
  (liii path)
  (liii string)
  (liii goldcheck unit-test source)
  (liii goldcheck unit-test source reexport)
) ;import

(check-set-mode! 'report-failed)

;; 1. 夹具测试：wrapper-lib 重新导出了 src-lib 的 real-fn，测试目录下错误地包含了 real-fn-test.scm
(let* ((base-dir (path->string (path-join (path-temp-dir) "gf-reexported-tests-fixture"))
       ) ;base-dir
       (test-dir (path->string (path-join base-dir "tests" "wrapper-lib")))
       (src-dir (path->string (path-join base-dir "goldfish")))
       (_ (path-mkdir (path test-dir) :parents #t :exist-ok #t))
       (_ (path-mkdir (path src-dir) :parents #t :exist-ok #t))
       (src-lib-file (path->string (path-join src-dir "src-lib.scm")))
       (wrapper-lib-file (path->string (path-join src-dir "wrapper-lib.scm")))
       (wrapper-test (path->string (path-join test-dir "wrapper-fn-test.scm")))
       (real-test (path->string (path-join test-dir "real-fn-test.scm")))
       (src-lib-content (string-append "(define-library (src-lib)\n"
                          "  (export real-fn)\n" "  (import (scheme base))\n"
                          "  (begin (define (real-fn) 42)))\n"
                        ) ;string-append
       ) ;src-lib-content
       (wrapper-lib-content (string-append "(define-library (wrapper-lib)\n"
                              "  (export wrapper-fn real-fn)\n"
                              "  (import (scheme base) (src-lib))\n"
                              "  (begin (define (wrapper-fn) 1)))\n"
                            ) ;string-append
       ) ;wrapper-lib-content
       (_ (path-write-bytes (path src-lib-file) (string->utf8 src-lib-content)))
       (_ (path-write-bytes (path wrapper-lib-file) (string->utf8 wrapper-lib-content))
       ) ;_
       (_ (path-write-bytes (path wrapper-test) (string->utf8 "(check 1 => 1)\n")))
       (_ (path-write-bytes (path real-test) (string->utf8 "(check 1 => 1)\n")))
       (viols (check-reexported-tests test-dir "tests/wrapper-lib"))
      ) ;
  (check-true (not (null? viols)))
  (check (length viols) => 1)
  (check (car (car viols)) => 'unit-test)
  (check (cadr (car viols)) => "tests/wrapper-lib/real-fn-test.scm")
  (check-true (string-contains? (list-ref (car viols) 3) "函数 real-fn 并非本库实现")
  ) ;check-true
  (check-true (string-contains? (list-ref (car viols) 3) "应移动到 tests/src-lib/ 目录")
  ) ;check-true

  ;; 清理
  (path-unlink real-test #t)
  (path-unlink wrapper-test #t)
  (path-unlink wrapper-lib-file #t)
  (path-unlink src-lib-file #t)
  (path-rmdir test-dir)
  (path-rmdir (path->string (path-parent (path test-dir))))
  (path-rmdir src-dir)
  (path-rmdir base-dir)
) ;let*

;; 2. 合规用例：全部测试均为本地实现函数的测试
(let* ((base-dir (path->string (path-join (path-temp-dir) "gf-reexported-good-fixture"))
       ) ;base-dir
       (test-dir (path->string (path-join base-dir "tests" "my-lib")))
       (src-dir (path->string (path-join base-dir "goldfish")))
       (_ (path-mkdir (path test-dir) :parents #t :exist-ok #t))
       (_ (path-mkdir (path src-dir) :parents #t :exist-ok #t))
       (src-file (path->string (path-join src-dir "my-lib.scm")))
       (foo-test (path->string (path-join test-dir "foo-test.scm")))
       (src-content (string-append "(define-library (my-lib)\n"
                      "  (export foo reexported-bar)\n"
                      "  (import (scheme base))\n"
                      "  (begin (define (foo) 1)))\n"
                    ) ;string-append
       ) ;src-content
       (_ (path-write-bytes (path src-file) (string->utf8 src-content)))
       (_ (path-write-bytes (path foo-test) (string->utf8 "(check 1 => 1)\n")))
       (viols (check-reexported-tests test-dir "tests/my-lib"))
      ) ;
  (check viols => '())
  (path-unlink foo-test #t)
  (path-unlink src-file #t)
  (path-rmdir test-dir)
  (path-rmdir (path->string (path-parent (path test-dir))))
  (path-rmdir src-dir)
  (path-rmdir base-dir)
) ;let*

(check-report)
