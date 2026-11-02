;; check-missing-exported-tests
;; 检查指定测试目录下是否覆盖了其对应库源文件导出的全部函数单元测试。
;;
;; 语法
;; ----
;; (check-missing-exported-tests parent rel-parent)
;;
;; 参数
;; ----
;; parent : string
;; 单元测试目录的完整或相对路径。
;; rel-parent : string
;; 单元测试目录相对于工程根的相对路径（用于生成违规报告）。
;;
;; 返回值
;; ------
;; list
;; 违规项列表。缺失导出函数单测时返回包含违规详情的列表；全部覆盖时返回空列表 '()。
;;
;; 说明
;; ----
;; 自动反查测试目录对应的源码库，提取全部 export 标识符并转换为标准测试文件名，逐一验证文件存在性。

(import (scheme base)
  (liii check)
  (liii path)
  (liii string)
  (liii goldcheck unit-test source)
) ;import

(check-set-mode! 'report-failed)

;; 1. 测试全部覆盖的已有核心库
(check (check-missing-exported-tests "tests/scheme/case-lambda" "tests/scheme/case-lambda") => '())

;; 2. 临时测试夹具：缺失导出函数单测
(let* ((base-dir (path->string (path-join (path-temp-dir) "gf-missing-tests-fixture")))
       (test-dir (path->string (path-join base-dir "tests" "my-lib")))
       (src-dir (path->string (path-join base-dir "goldfish")))
       (_ (path-mkdir (path test-dir) :parents #t :exist-ok #t))
       (_ (path-mkdir (path src-dir) :parents #t :exist-ok #t))
       (src-file (path->string (path-join src-dir "my-lib.scm")))
       (foo-test (path->string (path-join test-dir "foo-test.scm")))
       (src-content "(define-library (my-lib)\n  (export foo bar)\n  (import (scheme base))\n  (begin (define foo 1) (define bar 2)))\n")
       (_ (path-write-bytes (path src-file) (string->utf8 src-content)))
       (_ (path-write-bytes (path foo-test) (string->utf8 "(check 1 => 1)\n")))
       (viols (check-missing-exported-tests test-dir "tests/my-lib"))
      ) ;let*
  (check-true (not (null? viols)))
  (check (car (car viols)) => 'unit-test)
  (check-true (string-contains? (list-ref (car viols) 3) "缺少导出函数 bar 的单元测试"))
  (path-unlink foo-test #t)
  (path-unlink src-file #t)
  (path-rmdir test-dir)
  (path-rmdir (path->string (path-parent (path test-dir))))
  (path-rmdir src-dir)
  (path-rmdir base-dir)
) ;let*

(check-report)
