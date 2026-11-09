;; snake-dir-opposite?
;; 判断两个方向是否互为相反方向（180 度掉头）
;;
;; 语法
;; ----
;; (snake-dir-opposite? d1 d2)
;;
;; 参数
;; ----
;; d1, d2 : symbol?\n方向符号 up/down/left/right
;;
;; 返回值
;; ------
;; boolean?\n互为相反方向时返回 #t

(import (scheme base)
  (liii check)
  (liii tui snake logic)
) ;import

(check-set-mode! 'report-failed)

(check-true (snake-dir-opposite? 'up 'down))
(check-true (snake-dir-opposite? 'down 'up))
(check-true (snake-dir-opposite? 'left 'right))
(check-true (snake-dir-opposite? 'right 'left))
(check-false (snake-dir-opposite? 'up 'right))
(check-false (snake-dir-opposite? 'up 'up))

(check-report)
