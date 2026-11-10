;; skill-card-expanded?
;; 读取技能卡片当前展开状态（默认折叠）。
;;
;; 语法
;; ----
;; (skill-card-expanded? card) -> boolean
;;
;; 参数
;; ----
;; card : component?
;; 技能卡片组件。
;;
;; 返回值
;; ------
;; boolean
;; 展开时为 #t。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive views skill-card)
) ;import

(check-set-mode! 'report-failed)

(check-false (skill-card-expanded? (make-skill-card "a" "body")))
(check-true (skill-card-expanded? (make-skill-card "a" "body" :expanded? #t)))

(check-report)
