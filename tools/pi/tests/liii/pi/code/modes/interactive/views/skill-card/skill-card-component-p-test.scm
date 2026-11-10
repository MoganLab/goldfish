;; skill-card-component?
;; 判断 TUI 组件是否为技能卡片组件（component-data 持有 <skill-card>）。
;;
;; 语法
;; ----
;; (skill-card-component? obj) -> boolean
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ------
;; boolean
;; 是技能卡片组件时为 #t。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive views skill-card)
) ;import

(check-set-mode! 'report-failed)

(check-true (skill-card-component? (make-skill-card "a" "body")))
(check-false (skill-card-component? "plain"))
(check-false (skill-card-component? (make-container)))

(check-report)
