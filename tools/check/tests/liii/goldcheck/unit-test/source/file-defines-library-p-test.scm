;; file-defines-library?
;; 检查指定 Scheme 源文件中是否包含 define-library 库定义形式。
;;
;; 语法
;; ----
;; (file-defines-library? file-path)
;;
;; 参数
;; ----
;; file-path : string
;; 待检查的 Scheme 源文件路径。
;;
;; 返回值
;; ------
;; boolean?
;; 包含 (define-library ...) 时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 忽略注释并逐表达式扫描，若遇到文件不存在或读取错误则安全返回 #f。

(import (scheme base)
  (liii check)
  (liii path)
  (liii goldcheck unit-test source)
) ;import

(check-set-mode! 'report-failed)

;; 1. 存在 define-library 的库源文件
(check-true (file-defines-library? "r7rs/scheme/case-lambda.scm"))
(check-true (file-defines-library? "r7rs/scheme/base.scm"))

;; 2. 临时文件：无 define-library 的纯脚本
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-test-no-lib.scm"))))
  (path-write-bytes (path tmp-file) (string->utf8 "(define x 1)\n(+ x 2)\n"))
  (check (file-defines-library? tmp-file) => #f)
  (path-unlink tmp-file #t)
) ;let*

;; 3. 临时文件：包含 define-library
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-test-has-lib.scm"))))
  (path-write-bytes (path tmp-file)
    (string->utf8 ";; comment\n(define-library (foo bar) (export baz))\n")
  ) ;path-write-bytes
  (check (file-defines-library? tmp-file) => #t)
  (path-unlink tmp-file #t)
) ;let*

;; 4. 不存在的文件安全返回 #f
(check (file-defines-library? "path/to/nonexistent-file-12345.scm") => #f)

(check-report)
