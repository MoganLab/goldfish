(import (scheme base)
  (liii check)
  (liii string)
  (liii path)
  (liii goldcheck record-export)
) ;import

;; check-record-export
;; 检查 define-library 中记录类型句法标识 <xxx> 的错误导出与占位定义。
;; 规则一：<xxx> 不得出现在 (export ...) 中（含 rename 内外两个位置，
;; 即使 <xxx> 来自其他库的转发导出也违规）；
;; 规则二：(define <xxx> '<xxx>) 占位定义违规。
;;
;; 语法
;; ----
;; (check-record-export file-path)
;;
;; 参数
;; ----
;; file-path : string  待检查的 .scm 文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 每项形如 (list 'define-record-type rel-path line msg)。

(check-set-mode! 'report-failed)

(define %tmp-root (path->string (path-join (path-temp-dir) "gf-record-export-test")))

(define (%mkdirs p)
  (if (path-dir? p) #f (begin (%mkdirs (path-parent p)) (path-mkdir p)))
) ;define

(define (%mk name content)
  (let ((full (string-append %tmp-root "/" name)))
    (%mkdirs (path-parent (path full)))
    (call-with-port (open-output-file full) (lambda (p) (display content p)))
    full
  ) ;let
) ;define

;; 1. 干净文件：正常导出构造器/谓词 -> '()
(let ((f (%mk "clean.scm"
           "(define-library (demo clean)\n  (import (scheme base))\n  (export make-foo foo? foo-x)\n  (begin\n    (define-record-type <foo>\n      (make-foo x) foo? x foo-x)\n  ) ;begin\n) ;define-library\n"
         ) ;%mk
      ) ;f
     ) ;
  (check (check-record-export f) => '())
) ;let

;; 2. export <xxx> -> 1 条违规，报 export 子句行号
(let* ((f (%mk "bad-export.scm"
            "(define-library (demo bad)\n  (import (scheme base))\n  (export <foo> make-foo foo?)\n  (begin\n    (define-record-type <foo> (make-foo) foo?)\n  ) ;begin\n) ;define-library\n"
          ) ;%mk
       ) ;f
       (viol (check-record-export f))
      ) ;
  (check (length viol) => 1)
  (check (caar viol) => 'define-record-type)
  (check-true (string-ends? (cadr (car viol)) "/bad-export.scm"))
  (check (caddr (car viol)) => 3)
  (check-true (string-contains? (cadddr (car viol)) "<foo>"))
  (check-true (string-contains? (cadddr (car viol)) "should not be exported"))
) ;let*

;; 3. 占位定义 (define <xxx> '<xxx>) -> 1 条违规，报 define 行号
(let* ((f (%mk "placeholder.scm"
            "(define-library (demo ph)\n  (import (scheme base))\n  (export make-foo foo?)\n  (begin\n    (define-record-type <foo> (make-foo) foo?)\n    (define <foo> '<foo>)\n  ) ;begin\n) ;define-library\n"
          ) ;%mk
       ) ;f
       (viol (check-record-export f))
      ) ;
  (check (length viol) => 1)
  (check (caddr (car viol)) => 6)
  (check-true (string-contains? (cadddr (car viol)) "<foo>"))
  (check-true (string-contains? (cadddr (car viol)) "placeholder"))
) ;let*

;; 4. rename 内外两个位置的 <xxx> 均违规
(let* ((f (%mk "rename.scm"
            "(define-library (demo rn)\n  (import (scheme base))\n  (export make-foo (rename <foo> foo-t) (rename bar <bar>))\n) ;define-library\n"
          ) ;%mk
       ) ;f
       (viol (check-record-export f))
      ) ;
  (check (length viol) => 2)
  (check-true (string-contains? (cadddr (car viol)) "<foo>"))
  (check-true (string-contains? (cadddr (cadr viol)) "<bar>"))
) ;let*

;; 5. 形近但非 <xxx> 的符号放行：uri<? <= foo> <> < 等
(let ((f (%mk "ok-names.scm"
           "(define-library (demo ok)\n  (import (scheme base))\n  (export uri<? <= foo> <> < a)\n  (begin\n    (define x 'x)\n  ) ;begin\n) ;define-library\n"
         ) ;%mk
      ) ;f
     ) ;
  (check (check-record-export f) => '())
) ;let

;; 6. 转发导出：本库无 define-record-type 仍违规
(let* ((f (%mk "reexport.scm"
            "(define-library (demo reexport)\n  (import (scheme base) (demo other))\n  (export <foo> make-foo)\n) ;define-library\n"
          ) ;%mk
       ) ;f
       (viol (check-record-export f))
      ) ;
  (check (length viol) => 1)
  (check-true (string-contains? (cadddr (car viol)) "<foo>"))
) ;let*

;; 7. 非 define-library 文件（普通脚本）-> '()
(let ((f (%mk "script.scm" "(define x 1)\n(define y 2)\n")))
  (check (check-record-export f) => '())
) ;let

;; 8. 多条违规按文件出现顺序收集：2 条 export + 2 条占位
(let* ((f (%mk "multi.scm"
            "(define-library (demo multi)\n  (import (scheme base))\n  (export <a> make-a)\n  (export <b>)\n  (begin\n    (define <a> '<a>)\n    (define <b> '<b>)\n  ) ;begin\n) ;define-library\n"
          ) ;%mk
       ) ;f
       (viol (check-record-export f))
      ) ;
  (check (length viol) => 4)
  (check (caddr (car viol)) => 3)
  (check (caddr (cadr viol)) => 4)
  (check (caddr (caddr viol)) => 6)
  (check (caddr (cadddr viol)) => 7)
) ;let*

;; 9. 非自引用 quote 的 define 放行：(define <a> '<b>) 与 (define a 'a)
(let ((f (%mk "not-placeholder.scm"
           "(define-library (demo np)\n  (import (scheme base))\n  (begin\n    (define <a> '<b>)\n    (define a 'a)\n  ) ;begin\n) ;define-library\n"
         ) ;%mk
      ) ;f
     ) ;
  (check (check-record-export f) => '())
) ;let

;; ---- 清理临时目录 ---------------------------------------------------
(for-each
  (lambda (name) (path-unlink (path (string-append %tmp-root "/" name)) #t))
  '("clean.scm" "bad-export.scm" "placeholder.scm" "rename.scm"
    "ok-names.scm" "reexport.scm" "script.scm" "multi.scm" "not-placeholder.scm")
) ;for-each
(path-rmdir (path %tmp-root))

(check-report)
