;; pi-make-stream-progress-cb
;; 构造 libcurl progress 回调，在无数据期间按节拍探测中止信号与空闲超时。
;;
;; 语法
;; ----
;; (pi-make-stream-progress-cb eff-idle last-act on-idle on-abort)
;;
;; 参数
;; ----
;; eff-idle : number? | #f
;; 空闲超时秒数。
;; last-act : vector?
;; 记录上次活动单调时钟秒数的单元素向量。
;; on-idle : procedure?
;; 空闲超窗回调。
;; on-abort : procedure?
;; 用户中止回调。
;;
;; 返回值
;; ------
;; procedure?
;; 传给 http-post :progress 的回调函数。

(import (liii check) (liii pi ai abort) (liii pi ai time))

(check-set-mode! 'report-failed)

(pi-abort-reset!)
(let* ((idle? #f)
       (aborted? #f)
       (last-act (vector (pi-current-monotonic-seconds)))
       (cb (pi-make-stream-progress-cb 10
             last-act
             (lambda () (set! idle? #t))
             (lambda () (set! aborted? #t))
           ) ;pi-make-stream-progress-cb
       ) ;cb
      ) ;
  (check (cb 0 0 0 0) => #t)
  (check idle? => #f)
  (vector-set! last-act 0 (- (pi-current-monotonic-seconds) 20))
  (check (cb 0 0 0 0) => #f)
  (check idle? => #t)
) ;let*

(check-report)
