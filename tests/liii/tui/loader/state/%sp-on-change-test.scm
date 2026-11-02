;; %sp-on-change
;; 获取微调器内部记录的状态变更回调。
;;
;; 语法
;; ----
;; (%sp-on-change d)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; 返回值
;; ----
;; procedure? 或 #f
;; 回调过程或 #f。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let* ((cb (lambda (c) #t))
       (d (%make-spinner-data '("a") 0 "" "" "" 80 #t 'spinning cb #f))
      ) ;let*
  (check (%sp-on-change d) => cb)
) ;let*

(check-report)
