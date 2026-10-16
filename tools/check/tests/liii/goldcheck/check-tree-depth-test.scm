(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii goldcheck-rules)
) ;import

;; check-tree-depth
;; 逐顶级表达式度量语法树嵌套深度，并判断是否超出给定深度阈值。
;;
;; 语法
;; ----
;; (check-tree-depth file-path limit)
;;
;; 参数
;; ----
;; file-path : string
;; 待检查的文件路径。
;; limit : integer
;; 允许的最大嵌套深度上限。
;;
;; 返回值
;; ------
;; (values max-depth max-line violation-or-#f)
;; 第一个值为文件内各顶级表达式深度的最大值；
;; 第二个值为最深顶级表达式在文件中的起始行号；
;; 第三个值为违规结构 (list 'max-tree-depth file-path max-line max-depth limit)，未超限时为 #f。

(check-set-mode! 'report-failed)

;; 1. 深度恰好等于上限：深度 3，限制 3
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-check-depth-exact.scm")))
      ) ;
  (call-with-port (open-output-file tmp-file)
    (lambda (p) (display "(((1)))\n" p))
  ) ;call-with-port
  (call-with-values (lambda () (check-tree-depth tmp-file 3))
    (lambda (d l viol) (check d => 3) (check viol => #f))
  ) ;call-with-values
  (path-unlink tmp-file #t)
) ;let*

;; 2. 深度超出上限：深度 4，限制 3，验证行号定位
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-check-depth-over.scm")))
      ) ;
  (call-with-port (open-output-file tmp-file)
    (lambda (p) (display "(define a 1)\n\n;; 第 3 行最深\n((((1))))\n" p))
  ) ;call-with-port
  (call-with-values (lambda () (check-tree-depth tmp-file 3))
    (lambda (d l viol)
      (check d => 4)
      (check l => 4)
      (check (car viol) => 'max-tree-depth)
      (check (caddr viol) => 4)
      (check (cadddr viol) => 4)
      (check (list-ref viol 4) => 3)
    ) ;lambda
  ) ;call-with-values
  (path-unlink tmp-file #t)
) ;let*

;; 3. 字符串与注释不计入嵌套层数
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-check-depth-atom.scm")))
      ) ;
  (call-with-port (open-output-file tmp-file)
    (lambda (p) (display "(define msg \"((((nested string))))\")\n" p))
  ) ;call-with-port
  (call-with-values (lambda () (check-tree-depth tmp-file 3))
    (lambda (d l viol) (check d => 1) (check viol => #f))
  ) ;call-with-values
  (path-unlink tmp-file #t)
) ;let*

(check-report)
