;; check-file-lines
;; 统计单个文件的物理代码行数，并判断是否超出给定行数阈值（通过 (liii goldcheck) 导出）。
;;
;; 语法
;; ----
;; (check-file-lines file-path limit)
;;
;; 参数
;; ----
;; file-path : string   待统计的文件路径
;; limit : integer      允许的最大行数上限
;;
;; 返回值
;; ------
;; (values count violation-or-#f)
;; 第一个值为实际物理行数；第二个值为违规元组，未超限时为 #f。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii goldcheck)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正常文件不超限
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-lines-ok.scm"))))
  (call-with-port (open-output-file tmp) (lambda (p) (display "1\n2\n3\n" p)))
  (let-values (((count viol) (check-file-lines tmp 5)))
    (check count => 3)
    (check viol => #f)
  ) ;let-values
  (path-unlink (path tmp))
) ;let*

(check-report)
