(import (scheme base)
  (liii check)
  (liii pi code modes interactive app-model))

(check-set-mode! 'report-failed)

;; 命令识别：/model 与 /model <query>，排除 /models 等前缀误匹配。
(check-true (pi-app-model-command? "/model"))
(check-true (pi-app-model-command? "/model glm-5"))
(check-false (pi-app-model-command? "/models"))
(check-false (pi-app-model-command? "hello /model"))

(check-report)
