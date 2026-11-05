;; fix-file-dry-run
;; 预览模式：读取文件内容并输出修正后的源码，不写回磁盘。
;;
;; 语法
;; ----
;; (fix-file-dry-run path-str repair-source-fn)
;;
;; 参数
;; ----
;; path-str : string? 待处理的文件路径
;; repair-source-fn : procedure? 源码修正函数
;;
;; 返回值
;; ------
;; unspecified 预览结果打印到当前输出端口

(import (scheme base)
  (liii check)
  (liii base)
  (liii path)
  (liii string)
  (liii tempfile)
  (liii goldfix)
  (liii goldfix batch)
) ;import

(check-set-mode! 'report-failed)

;; 测试：输出修正后的源码，且原文件保持不变
(with-temp-file
  (lambda (file)
    (path-write-text (path file) "(define x 1\n")
    (let* ((out
             (with-output-to-string
               (lambda () (fix-file-dry-run file (lambda (src) (string-append src ")"))))
             ) ;with-output-to-string
           ) ;out
          ) ;
      (check-true (string-contains? out "(define x 1\n)"))
      (check (path-read-text (path file)) => "(define x 1\n")
    ) ;let*
  ) ;lambda
  "goldfix-dry-run"
  ".scm"
) ;with-temp-file

(check-report)
