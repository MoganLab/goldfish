(import (liii check)
        (scheme complex)
) ;import

(check-set-mode! 'report-failed)

;; 本文件为 (scheme complex) 的入口说明文件
;; 实际测试用例位于 tests/scheme/complex/ 目录下

;; 导入所有子模块测试
(load "tests/scheme/complex/real-part-test.scm")
(load "tests/scheme/complex/imag-part-test.scm")
(load "tests/scheme/complex/angle-test.scm")
(load "tests/scheme/complex/magnitude-test.scm")
(load "tests/scheme/complex/make-polar-test.scm")
(load "tests/scheme/complex/make-rectangular-test.scm")

(check-report)
