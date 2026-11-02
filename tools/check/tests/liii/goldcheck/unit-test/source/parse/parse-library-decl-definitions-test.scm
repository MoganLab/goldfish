;; parse-library-decl-definitions
;; 从库声明表单（如 begin 或 include）中提取定义的标识符名称列表。
;;
;; 语法
;; ----
;; (parse-library-decl-definitions decl source-dir)
;;
;; 参数
;; ----
;; decl : pair
;; 库声明表单，如 (begin (define foo 1)) 或 (include "sub.scm")。
;; source-dir : string
;; 源码文件所在目录，用于定位 include 的相对文件。
;;
;; 返回值
;; ------
;; list of string
;; 声明中包含的所有定义标识符名称字符串列表。

(import (scheme base)
  (liii check)
  (liii path)
  (liii goldcheck unit-test source parse)
) ;import

(check-set-mode! 'report-failed)

;; 1. 非 begin 或 include 声明返回空列表
(check (parse-library-decl-definitions '(export a b) ".") => '())
(check (parse-library-decl-definitions '(import (scheme base)) ".") => '())

;; 2. 解析 begin 声明中的定义
(check
  (parse-library-decl-definitions
    '(begin (define a 1) (define (b x) x) (define-syntax s (syntax-rules ())))
    "."
  ) ;parse-library-decl-definitions
  =>
  '("s" "b" "a")
) ;check

;; 3. 解析 include 声明引用的文件定义
(let* ((tmp-dir (path-temp-dir))
       (inc-file (path->string (path-join tmp-dir "gf-test-inc.scm")))
       (content "(define inc-var 42)\n(define (inc-fn) #t)\n")
      ) ;
  (path-write-bytes (path inc-file) (string->utf8 content))
  (let ((defs (parse-library-decl-definitions
                '(include "gf-test-inc.scm")
                (path->string tmp-dir)
              ) ;parse-library-decl-definitions
        ) ;defs
       ) ;
    (check defs => '("inc-fn" "inc-var"))
  ) ;let
  (path-unlink inc-file #t)
) ;let*

(check-report)
