;; check-no-glue
;; 检查单文件中 glue 是否只在 define-library 中使用且未被直接导出。
;;
;; 语法
;; ----
;; (check-no-glue file-path)
;;
;; 参数
;; ----
;; file-path : string   待检查的文件路径
;;
;; 返回值
;; ------
;; list of violation
;; 违规信息列表，合规时返回空列表。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii goldcheck no-glue)
) ;import

(check-set-mode! 'report-failed)

;; 1. 普通纯 Scheme 库无违规
(let* ((tmp (path->string (path-join (path-temp-dir) "gf-test-no-glue-pure.scm")))
       (content "(define-library (foo bar)\n  (import (scheme base))\n  (export x)\n  (begin (define x 1)))\n"
       ) ;content
      ) ;
  (call-with-output-file tmp (lambda (p) (write-string content p)))
  (check (check-no-glue tmp) => '())
  (path-unlink (path tmp))
) ;let*

(check-report)
