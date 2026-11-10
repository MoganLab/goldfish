;; find-repo-root
;; 获取仓库根目录：优先 g_project-root，不可用时从 cwd 向上探测。
;;
;; 语法
;; ----
;; (find-repo-root)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; string 或 #f
;; 仓库根目录路径；找不到时返回 #f。

(import (scheme base)
  (liii check)
  (liii os)
  (liii path)
  (liii goldcheck type-hint hint-data)
) ;import

(check-set-mode! 'report-failed)

;; 1. 仓库内执行：返回包含 goldfish/ 与 gf_check.json 的根
(let ((root (find-repo-root)))
  (check-true (and root (string? root)))
  (when root
    (check-true (path-dir? (path (string-append root "/goldfish"))))
    (check-true (path-file? (path (string-append root "/gf_check.json"))))
  ) ;when
) ;let

(check-report)
