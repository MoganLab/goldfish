;; %sp-on-change-set!
;; 设置微调器内部记录的状态变更回调。
;;
;; 语法
;; ----
;; (%sp-on-change-set! d cb)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; cb : procedure? 或 #f
;; 回调过程或 #f。
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

(let* ((cb (lambda (c) #t))
       (d (%make-spinner-data '("a") 0 "" "" "" 80 #t 'spinning #f #f))
      ) ;let*
  (%sp-on-change-set! d cb)
  (check (%sp-on-change d) => cb)
) ;let*

(check-report)
