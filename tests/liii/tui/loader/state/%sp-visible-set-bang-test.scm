;; %sp-visible-set!
;; 设置微调器内部记录的可见性。
;;
;; 语法
;; ----
;; (%sp-visible-set! d visible?)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; visible? : boolean?
;; 是否可见。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-spinner-data '("a") 0 "" "" "" 80 #t 'spinning #f #f)))
  (%sp-visible-set! d #f)
  (check-false (%sp-visible d))
) ;let

(check-report)
