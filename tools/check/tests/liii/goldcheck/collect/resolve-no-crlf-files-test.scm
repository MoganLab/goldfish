;; resolve-no-crlf-files
;; 为 no-crlf 规则解析匹配配置后缀与路径的目标文件列表。
;;
;; 语法
;; ----
;; (resolve-no-crlf-files parser cfg)
;;
;; 参数
;; ----
;; parser : procedure  命令行参数解析器对象
;; cfg : list          检查配置（JSON alist 结构）
;;
;; 返回值
;; ------
;; list of string
;; 符合后缀及路径范围要求的待检测文件列表。

(import (scheme base) (liii check) (liii string) (liii goldcheck collect))

(check-set-mode! 'report-failed)

;; 1. 指定位置参数时的过滤
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
       (cfg
         '(("rules" ("no-crlf" ("suffix" . #("scm" "json")))))
       ) ;cfg
       (files (resolve-no-crlf-files mock-parser cfg))
      ) ;
  (check (length files) => 1)
  (check-true (string-ends? (car files) "collect.scm"))
) ;let*

(check-report)
