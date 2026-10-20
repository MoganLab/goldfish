(import (liii check) (liii string) (liii pi))

(check-set-mode! 'report-failed)

;; pi-build-system-prompt
;; 构建包含激活工具、运行环境、上下文以及会话预算约束的完整系统提示词。

;; 1. 默认预算参数（session-timeout 600, max-turns 200）
(let ((sp (pi-build-system-prompt '("read" "bash") "/test/cwd" #f #f '())))
  (check-true (string-contains? sp "You are an expert coding assistant operating inside pi"))
  (check-true (string-contains? sp "<cwd>\n/test/cwd\n</cwd>"))
  (check-true (string-contains? sp "<session_budget>"))
  (check-true (string-contains? sp "Session Timeout: 600 seconds (hard wall-clock deadline)"))
  (check-true (string-contains? sp "Max Turns: 200 turns."))
  (check-true (string-contains? sp "Pacing: Pace your work accordingly."))
  (check-true (string-contains? sp "</session_budget>")))

;; 2. 自定义有限预算（例如 1800s, 50 轮）
(let ((sp (pi-build-system-prompt '("read" "bash") "/test/cwd" #f #f '() '() 1800 50)))
  (check-true (string-contains? sp "Session Timeout: 1800 seconds"))
  (check-true (string-contains? sp "Max Turns: 50 turns.")))

;; 3. 无硬性超时与无限轮次（session-timeout #f / 0, max-turns 0）
(let ((sp (pi-build-system-prompt '("read") "/test/cwd" #f #f '() '() 0 0)))
  (check-true (string-contains? sp "Session Timeout: None (no wall-clock deadline configured)."))
  (check-true (string-contains? sp "Max Turns: Unlimited (no turn limit configured).")))

(let ((sp (pi-build-system-prompt '("read") "/test/cwd" #f #f '() '() #f #f)))
  (check-true (string-contains? sp "Session Timeout: None (no wall-clock deadline configured)."))
  (check-true (string-contains? sp "Max Turns: Unlimited (no turn limit configured).")))

(check-report)
