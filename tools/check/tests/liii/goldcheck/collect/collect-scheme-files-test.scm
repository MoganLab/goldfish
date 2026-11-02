;; collect-scheme-files
;; 递归扫描指定目录收集 Scheme (.scm) 文件。
;;
;; 语法
;; ----
;; (collect-scheme-files dir-path)
;;
;; 参数
;; ----
;; dir-path : string  目录路径
;;
;; 返回值
;; ------
;; list of string
;; 收集到的 .scm 文件相对路径列表。

(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii goldcheck collect)
) ;import

(check-set-mode! 'report-failed)

(let* ((dir (if (file-exists? "tools/check/liii/goldcheck")
              "tools/check/liii/goldcheck"
              "liii/goldcheck"
            ) ;if
       ) ;dir
       (files (collect-scheme-files dir))
      ) ;
  (check-true (list? files))
  (check-true (> (length files) 0))
  (check-true (every (lambda (f) (string-ends? f ".scm")) files))
) ;let*

(check-report)
