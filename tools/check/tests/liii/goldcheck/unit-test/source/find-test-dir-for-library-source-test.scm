;; find-test-dir-for-library-source
;; 根据 Scheme 库源码文件路径自动推断其对应的单元测试目录。
;;
;; 语法
;; ----
;; (find-test-dir-for-library-source src-path)
;;
;; 参数
;; ----
;; src-path : string
;; Scheme 源码文件路径。
;;
;; 返回值
;; ------
;; string? | #f
;; 成功推断时返回对应单元测试目录路径；若源码文件未定义库或无法推断则返回 #f。
;;
;; 说明
;; ----
;; 规则：
;; 1. 板块库：r7rs/<subpath>.scm -> r7rs/tests/<subpath>
;;            library/<subpath>.scm -> library/tests/<subpath>
;;            goldfish/<subpath>.scm -> tests/<subpath>
;; 2. 工具库：tools/<tool>/<subpath>.scm -> tools/<tool>/tests/<subpath>
;; 3. 若文件不包含 define-library 或本身已是测试目录下的文件，返回 #f。

(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii goldcheck path)
  (liii goldcheck unit-test source)
) ;import

(check-set-mode! 'report-failed)

;; 1. R7RS 板块库路径推断
(let ((dir (find-test-dir-for-library-source "r7rs/scheme/case-lambda.scm")))
  (check-true (string? dir))
  (check-true (string-ends? (normalize-sep dir) "r7rs/tests/scheme/case-lambda"))
) ;let

;; 2. 工具库路径推断
(let ((dir (find-test-dir-for-library-source "tools/check/liii/goldcheck/rules.scm"))
     ) ;
  (check-true (string? dir))
  (check-true (string-ends? (normalize-sep dir) "tools/check/tests/liii/goldcheck/rules")
  ) ;check-true
) ;let

;; 3. 非库源文件返回 #f
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-test-not-a-lib.scm"))))
  (path-write-bytes (path tmp-file) (string->utf8 "(display \"not a lib\")\n"))
  (check (find-test-dir-for-library-source tmp-file) => #f)
  (path-unlink tmp-file #t)
) ;let*

;; 4. 测试文件本身不推断测试目录
(check (find-test-dir-for-library-source "r7rs/tests/scheme/case-lambda/case-lambda-test.scm"
       ) ;find-test-dir-for-library-source
  =>
  #f
) ;check

(check-report)
