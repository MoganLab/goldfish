(import (liii check) (liii string) (liii pi))

(check-set-mode! 'report-failed)

;; pi-build-system-prompt
;; 构建包含激活工具、运行环境与上下文的完整系统提示词。
(let ((sp (pi-build-system-prompt '("read" "bash") "/test/cwd" #f #f '())))
  (check-true (string-contains? sp "You are an expert coding assistant operating inside pi"))
  (check-true (string-contains? sp "<cwd>\n/test/cwd\n</cwd>")))

(check-report)
