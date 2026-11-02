;; resolve-repo-file
;; 将相对于仓库根目录的路径解析为完整的绝对路径。
;;
;; 语法
;; ----
;; (resolve-repo-file rel)
;;
;; 参数
;; ----
;; rel : string
;; 相对或绝对文件路径。
;;
;; 返回值
;; ------
;; string
;; 解析后的绝对路径字符串；若 rel 已为绝对路径则原样返回。

(import (scheme base)
  (liii check)
  (liii path)
  (liii string)
  (liii goldcheck unit-test source parse)
) ;import

(check-set-mode! 'report-failed)

;; 1. 绝对路径原样返回
(let ((abs-path (path->string (path-temp-dir))))
  (check (resolve-repo-file abs-path) => abs-path)
) ;let

;; 2. 相对路径拼接到仓库根目录且可访问
(let ((resolved (resolve-repo-file "goldfish/scheme/base.scm")))
  (check-true (string? resolved))
  (check-true (path-absolute? (path resolved)))
  (check-true (path-file? (path resolved)))
) ;let

(check-report)
