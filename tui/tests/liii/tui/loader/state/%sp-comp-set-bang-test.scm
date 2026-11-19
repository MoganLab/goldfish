;; %sp-comp-set!
;; 设置微调器内部记录所属的外层组件对象。
;;
;; 语法
;; ----
;; (%sp-comp-set! d comp)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; comp : component? 或 #f
;; 外层组件对象。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let* ((s (make-spinner "test"))
       (d (%unwrap-spinner s "test"))
      ) ;let*
  (%sp-comp-set! d #f)
  (check-false (%sp-comp d))
) ;let*

(check-report)
