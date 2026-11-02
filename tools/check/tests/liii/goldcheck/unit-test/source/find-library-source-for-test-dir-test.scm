;; find-library-source-for-test-dir
;; 根据单元测试目录路径反向查找其对应的库源码实现文件路径。
;;
;; 语法
;; ----
;; (find-library-source-for-test-dir parent)
;;
;; 参数
;; ----
;; parent : string
;; 单元测试目录路径。
;;
;; 返回值
;; ------
;; string? | #f
;; 成功查找到对应源文件时返回其路径字符串，否则返回 #f。
;;
;; 说明
;; ----
;; 匹配规则：
;; 1. 工具测试目录：tools/<tool>/tests/<subpath> -> tools/<tool>/<subpath>.scm
;; 2. 核心测试目录：tests/<subpath> -> goldfish/<subpath>.scm
;; 3. 测试夹具目录：<prefix>/tests/<subpath> -> <prefix>/goldfish/<subpath>.scm

(import (scheme base)
  (liii check)
  (liii string)
  (liii goldcheck path)
  (liii goldcheck unit-test source)
) ;import

(check-set-mode! 'report-failed)

;; 1. 核心库测试目录反推源码
(let ((src (find-library-source-for-test-dir "tests/scheme/case-lambda")))
  (check-true (string? src))
  (check-true (string-ends? (normalize-sep src) "goldfish/scheme/case-lambda.scm")
  ) ;check-true
) ;let

;; 2. 工具库测试目录反推源码
(let ((src (find-library-source-for-test-dir "tools/check/tests/liii/goldcheck/rules")
      ) ;src
     ) ;
  (check-true (string? src))
  (check-true (string-ends? (normalize-sep src) "tools/check/liii/goldcheck/rules.scm")
  ) ;check-true
) ;let

;; 3. 无对应源码的目录返回 #f
(check (find-library-source-for-test-dir "tests/nonexistent-lib-dir") => #f)

(check-report)
