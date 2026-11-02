;; collect-positional-files
;; 收集位置参数（文件或目录）下的匹配文件相对路径。
;;
;; 语法
;; ----
;; (collect-positional-files positionals . maybe-suffixes)
;;
;; 参数
;; ----
;; positionals : list of string     位置参数列表（路径或文件）
;; maybe-suffixes : list of string  可选后缀名列表，默认为 '("scm")
;;
;; 返回值
;; ------
;; list of string
;; 收集到的符合后缀要求的文件列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldcheck collect)
) ;import

(check-set-mode! 'report-failed)

;; 1. 指定文件
(let* ((target (if (file-exists? "tools/check/liii/goldcheck/collect.scm")
                 "tools/check/liii/goldcheck/collect.scm"
                 "liii/goldcheck/collect.scm"
               ) ;if
       ) ;target
       (files (collect-positional-files (list target)))
      ) ;
  (check (length files) => 1)
  (check-true (string-ends? (car files) "collect.scm"))
) ;let*

;; 2. 指定目录并自定义后缀
(let* ((target (if (file-exists? "tools/check/tests/liii/goldcheck/collect")
                 "tools/check/tests/liii/goldcheck/collect"
                 "tests/liii/goldcheck/collect"
               ) ;if
       ) ;target
       (files (collect-positional-files (list target) '("md")))
      ) ;
  (check-true (list? files))
  (check-true (> (length files) 0))
  (check-true (every (lambda (f) (string-ends? f ".md")) files))
) ;let*

(check-report)
