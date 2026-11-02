;; collect-files-by-suffixes
;; 递归扫描指定目录收集匹配后缀名列表的文件相对路径。
;;
;; 语法
;; ----
;; (collect-files-by-suffixes dir-path suffixes)
;;
;; 参数
;; ----
;; dir-path : string       目录路径
;; suffixes : list of string  后缀名列表（不含点）
;;
;; 返回值
;; ------
;; list of string
;; 匹配到的文件相对路径列表。

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
       (files (collect-files-by-suffixes dir '("scm")))
      ) ;
  (check-true (list? files))
  (check-true (> (length files) 0))
  (check-true (any (lambda (f) (string-contains? f "cache.scm")) files))
) ;let*

(check-report)
