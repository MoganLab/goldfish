;; %notify-spinner-change!
;; 触发微调器状态变更回调。
;;
;; 语法
;; ----
;; (%notify-spinner-change! sp)
;;
;; 参数
;; ----
;; sp : %spinner-data?
;; 内部微调器数据对象。
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

(let ((called #f))
  (let* ((s (make-spinner "test" :on-change (lambda (c) (set! called #t))))
         (sp (%unwrap-spinner s "test"))
        ) ;
    (%notify-spinner-change! sp)
    (check-true called)
  ) ;let*
) ;let

(check-report)
