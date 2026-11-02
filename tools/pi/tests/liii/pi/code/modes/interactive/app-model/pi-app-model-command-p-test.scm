;; pi-app-model-command?
;; 判定对象是否满足 pi-app-model-command? 条件。
;;
;; 语法
;; ----
;; (pi-app-model-command? ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; boolean?
;; 满足条件返回 #t，否则返回 #f。

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
