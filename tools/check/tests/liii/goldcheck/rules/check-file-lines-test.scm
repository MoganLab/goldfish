(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii goldcheck rules)
) ;import

;; check-file-lines
;; 统计单个文件的物理代码行数，并判断是否超出给定行数阈值。
;;
;; 语法
;; ----
;; (check-file-lines file-path limit)
;;
;; 参数
;; ----
;; file-path : string
;; 待统计的文件路径。
;; limit : integer
;; 允许的最大行数上限。
;;
;; 返回值
;; ------
;; (values count violation-or-#f)
;; 第一个值为实际物理行数（含空行与注释）；
;; 第二个值为违规结构 (list 'max-file-lines file-path count limit)，未超限时为 #f。

(check-set-mode! 'report-failed)

;; 1. 正常文件：3 行代码，限制 5 行
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-check-lines-ok.scm"))))
  (call-with-port (open-output-file tmp-file)
    (lambda (p) (display ";; line 1\n(define a 1)\n(define b 2)\n" p))
  ) ;call-with-port
  (call-with-values (lambda () (check-file-lines tmp-file 5))
    (lambda (count viol) (check count => 3) (check viol => #f))
  ) ;call-with-values
  (path-unlink tmp-file #t)
) ;let*

;; 2. 刚好等于上限：3 行代码，限制 3 行
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-check-lines-exact.scm")))
      ) ;
  (call-with-port (open-output-file tmp-file)
    (lambda (p) (display "1\n2\n3\n" p))
  ) ;call-with-port
  (call-with-values (lambda () (check-file-lines tmp-file 3))
    (lambda (count viol) (check count => 3) (check viol => #f))
  ) ;call-with-values
  (path-unlink tmp-file #t)
) ;let*

;; 3. 超过上限：4 行代码，限制 3 行
(let* ((tmp-file (path->string (path-join (path-temp-dir) "gf-check-lines-over.scm")))
      ) ;
  (call-with-port (open-output-file tmp-file)
    (lambda (p) (display "1\n2\n3\n4\n" p))
  ) ;call-with-port
  (call-with-values (lambda () (check-file-lines tmp-file 3))
    (lambda (count viol)
      (check count => 4)
      (check (car viol) => 'max-file-lines)
      (check (caddr viol) => 4)
      (check (cadddr viol) => 3)
    ) ;lambda
  ) ;call-with-values
  (path-unlink tmp-file #t)
) ;let*

(check-report)
