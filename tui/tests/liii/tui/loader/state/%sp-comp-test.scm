;; %sp-comp
;; 获取微调器内部记录所属的外层组件对象。
;;
;; 语法
;; ----
;; (%sp-comp d)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; 返回值
;; ----
;; component? 或 #f
;; 外层组件对象或 #f。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let* ((s (make-spinner "test"))
       (d (%unwrap-spinner s "test"))
      ) ;let*
  (check (eq? (%sp-comp d) s) => #t)
) ;let*

(check-report)
