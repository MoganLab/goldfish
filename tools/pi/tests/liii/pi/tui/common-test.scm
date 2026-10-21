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

;; 2. 测试 detect-git-branch 在当前仓库下能够探测到分支名称
(let ((branch (detect-git-branch)))
  (check-true (or (not branch) (string? branch)))
  (check-true (string? branch))
) ;let

(check-report)
