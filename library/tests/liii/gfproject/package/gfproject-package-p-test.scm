;; gfproject-package?
;; 判断对象是否为 gfproject package 元数据记录。
;;
;; 语法
;; ----
;; (gfproject-package? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; package 记录返回 #t，其他对象返回 #f。

(import (liii check) (liii gfproject package))

(check-set-mode! 'report-failed)
(check-true (gfproject-package? (gfproject-package "lang" "liii")))
(check-false (gfproject-package? '(("name" . "lang"))))
(check-false (gfproject-package? #f))

(check-report)
