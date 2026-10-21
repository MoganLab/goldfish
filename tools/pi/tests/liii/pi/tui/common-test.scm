(import (scheme base)
  (liii check)
  (liii string)
  (liii pi tui common)
) ;import

(check-set-mode! 'report)

;; 1. 测试 %keyword-value
(check (%keyword-value :key '(:key "val") #f) => "val")
(check (%keyword-value :missing '(:key "val") "default") => "default")
(check (%keyword-value :key '() "default") => "default")

;; 2. 测试 detect-git-branch 返回合法分支名或 #f（在 detached HEAD / CI 环境下为 #f）
(let ((branch (detect-git-branch)))
  (check-true (or (not branch) (string? branch)))
  (when (string? branch)
    (check-true (not (string-null? branch)))
  ) ;when
) ;let

(check-report)
