;; skill-card?
;; 判断对象是否为 <skill-card> 技能卡片记录（组件内层数据）。
;;
;; 语法
;; ----
;; (skill-card? obj) -> boolean
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ------
;; boolean
;; 是技能卡片记录时为 #t。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii pi code modes interactive views skill-card)
) ;import

(check-set-mode! 'report-failed)

(check-true (skill-card? (component-data (make-skill-card "a" "body"))))
(check-false (skill-card? "not-a-card"))
(check-false (skill-card? #f))

(check-report)
