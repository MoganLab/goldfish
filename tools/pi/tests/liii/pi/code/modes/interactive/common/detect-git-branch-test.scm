(import (scheme base)
  (liii check)
  (liii string)
  (liii pi code modes interactive common))

(check-set-mode! 'report)

;; 返回合法分支名或 #f（在 detached HEAD / CI 环境下为 #f）
(let ((branch (detect-git-branch)))
  (check-true (or (not branch) (string? branch)))
  (when (string? branch)
    (check-true (not (string-null? branch)))))

(check-report)
