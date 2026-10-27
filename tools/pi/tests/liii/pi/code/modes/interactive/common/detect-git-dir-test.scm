(import (scheme base)
  (liii check)
  (liii string)
  (liii pi code modes interactive common))

(check-set-mode! 'report)

;; 返回 .git 目录绝对路径或 #f（非 git 仓库 / git 缺失）
(let ((d (detect-git-dir)))
  (check-true (or (not d) (and (string? d) (not (string-null? d)))))
  (when d
    (check-true (string-contains? d ".git"))))

(check-report)
