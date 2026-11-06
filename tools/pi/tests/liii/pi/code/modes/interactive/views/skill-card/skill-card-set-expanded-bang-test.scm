;; skill-card-set-expanded!
;; 设置技能卡片展开状态。
;;
;; 语法
;; ----
;; (skill-card-set-expanded! card flag)
;;
;; 参数
;; ----
;; card : component?
;; 技能卡片组件。
;; flag : boolean
;; 目标展开状态。
;;
;; 返回值
;; ------
;; 未指定

(import (scheme base)
  (liii check)
  (liii pi code modes interactive views skill-card)
) ;import

(check-set-mode! 'report-failed)

(let ((card (make-skill-card "a" "body")))
  (check-false (skill-card-expanded? card))
  (skill-card-set-expanded! card #t)
  (check-true (skill-card-expanded? card))
  (skill-card-set-expanded! card #f)
  (check-false (skill-card-expanded? card))) ;let

(check-report)
