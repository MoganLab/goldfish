;; collect-repo-files
;; 扫描整个代码仓库根目录下的全部 Scheme (.scm) 文件。
;;
;; 语法
;; ----
;; (collect-repo-files)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; list of string
;; 仓库内所有符合规范的 Scheme 文件路径列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldcheck collect)
) ;import

(check-set-mode! 'report-failed)

(let ((files (collect-repo-files)))
  (check-true (list? files))
  (check-true (> (length files) 0))
  (check-true (every (lambda (f) (string-ends? f ".scm")) files))
) ;let

(check-report)
