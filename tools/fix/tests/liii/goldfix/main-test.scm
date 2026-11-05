;; main
;; gf fix 子命令入口：解析命令行参数并执行括号修正。
;; 可显式传入参数列表（供单元测试驱动），未给出时从进程命令行解析。
;;
;; 语法
;; ----
;; (main)
;; (main args ...)
;;
;; 参数
;; ----
;; args ... : string?
;; 可选的显式参数列表；未给出时从 (command-line) 解析。
;;
;; 返回值
;; ------
;; boolean? 处理完成返回 #t。

(import (scheme base)
  (liii check)
  (liii base)
  (liii path)
  (liii string)
  (liii tempfile)
  (liii goldfix)
) ;import

(check-set-mode! 'report-failed)

;; 测试：--help 输出帮助文档并返回 #t
(let ((out "") (result #f))
  (set! out (with-output-to-string (lambda () (set! result (main '("--help"))))))
  (check result => #t)
  (check-true (string-contains? out "Usage: gf fix"))
) ;let

;; 测试：--dry-run 只输出预览结果，不修改文件
(with-temp-file
  (lambda (file)
    (path-write-text (path file) "(define x 1")
    (let ((out ""))
      (set! out (with-output-to-string
                   (lambda () (main (list "--dry-run" file)))
                 ) ;with-output-to-string
              ) ;
      (check-true (string-contains? out "(define x 1)"))
      (check (path-read-text (path file)) => "(define x 1")
    ) ;let
  ) ;lambda
  "goldfix-main"
  ".scm"
) ;with-temp-file

(check-report)
