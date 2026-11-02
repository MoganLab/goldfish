;; check-tree-depth
;; 计算单文件内 S 表达式语法树的最大嵌套深度，并判断是否超出限制阈值（通过 (liii goldcheck) 导出）。
;;
;; 语法
;; ----
;; (check-tree-depth file-path limit)
;;
;; 参数
;; ----
;; file-path : string   待检查的文件路径
;; limit : integer      允许的最大深度上限
;;
;; 返回值
;; ------
;; (values max-depth max-line violation-or-#f)
;; 第一个值为实际最大深度；第二个值为行号；第三个值为违规元组，未超限时为 #f。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii goldcheck)
) ;import

(check-set-mode! 'report-failed)

;; 1. 简单表达式深度不超限
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-depth-ok.scm"))))
  (call-with-port (open-output-file tmp)
    (lambda (p) (display "(define a 1)\n" p))
  ) ;call-with-port
  (call-with-values (lambda () (check-tree-depth tmp 5))
    (lambda (depth line viol) (check depth => 1) (check viol => #f))
  ) ;call-with-values
  (path-unlink (path tmp))
) ;let*

(check-report)
