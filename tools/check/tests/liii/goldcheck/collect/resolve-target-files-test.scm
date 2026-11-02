;; resolve-target-files
;; 根据命令行参数解析器状态解析待检查的 Scheme 目标文件列表。
;;
;; 语法
;; ----
;; (resolve-target-files parser)
;;
;; 参数
;; ----
;; parser : procedure  命令行参数解析器对象（支持 'changed-since 与 :positionals）
;;
;; 返回值
;; ------
;; list of string
;; 解析出的目标 Scheme 文件列表。

(import (scheme base) (liii check) (liii string) (liii goldcheck collect))

(check-set-mode! 'report-failed)

;; 1. 指定位置参数时的解析
(let* ((target (if (file-exists? "tools/check/liii/goldcheck/collect.scm")
                 "tools/check/liii/goldcheck/collect.scm"
                 "liii/goldcheck/collect.scm"
               ) ;if
       ) ;target
       (mock-parser
         (lambda (msg)
           (cond ((eq? msg 'changed-since) #f)
                 ((eq? msg :positionals) (list target))
                 (else #f)
           ) ;cond
         ) ;lambda
       ) ;mock-parser
       (files (resolve-target-files mock-parser))
      ) ;
  (check (length files) => 1)
  (check-true (string-ends? (car files) "collect.scm"))
) ;let*

(check-report)
