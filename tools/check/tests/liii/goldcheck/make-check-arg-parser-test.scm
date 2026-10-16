(import (scheme base) (liii check) (liii goldcheck))

;; make-check-arg-parser
;; 构造 gf check 子命令专用的命令行参数解析器。
;;
;; 语法
;; ----
;; (make-check-arg-parser)
;;
;; 返回值
;; ------
;; argument-parser 实例。

(check-set-mode! 'report-failed)

;; 1. 解析 --help
(let ((parser (make-check-arg-parser)))
  (parser :parse-argv '("gf" "check" "--help"))
  (check (parser 'help) => #t)
) ;let

;; 2. 解析 --changed-since
(let ((parser (make-check-arg-parser)))
  (parser :parse-argv '("gf" "check" "--changed-since=main"))
  (check (parser 'changed-since) => "main")
) ;let

;; 3. 解析位置参数
(let ((parser (make-check-arg-parser)))
  (parser :parse-argv '("gf" "check" "foo.scm" "bar/"))
  (let ((pos (parser :positionals)))
    (check (length pos) => 2)
    (check (car pos) => "foo.scm")
    (check (cadr pos) => "bar/")
  ) ;let
) ;let

(check-report)
