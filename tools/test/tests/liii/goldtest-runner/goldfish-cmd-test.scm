;; goldfish-cmd
;; TODO: 补充 goldfish-cmd 的简要描述。
;;
;; 语法
;; ----
;; (goldfish-cmd ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii string) (liii goldtest-runner))

(check-set-mode! 'report-failed)

(check-true (procedure? goldfish-cmd))
(check-true (string-contains (goldfish-cmd "bignum/tests/liii/bignum/eq-test.scm")
              "-m r7rs+bignum"
            ) ;string-contains
) ;check-true
(check-true (string-contains (goldfish-cmd "r7rs/tests/scheme/base/abs-test.scm") "-m r7rs ")
) ;check-true
(check-true (string-contains (goldfish-cmd "tools/fmt/tests/liii/goldfmt/dir-test.scm")
              "-m r7rs -I tools/fmt "
            ) ;string-contains
) ;check-true
(check-false (string-contains (goldfish-cmd "tests/liii/goldtest-runner/goldfish-cmd-test.scm")
               "-I "
             ) ;string-contains
) ;check-false

(check-report)
