;; %sp-frame-idx-set!
;; 设置微调器内部记录的当前帧索引。
;;
;; 语法
;; ----
;; (%sp-frame-idx-set! d idx)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; idx : integer?
;; 目标帧索引。
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
  (%sp-frame-idx-set! d 2)
  (check (%sp-frame-idx d) => 2)
) ;let

(check-report)
