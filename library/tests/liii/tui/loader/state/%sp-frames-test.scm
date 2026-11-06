;; %sp-frames
;; 获取微调器内部状态记录的帧列表。
;;
;; 语法
;; ----
;; (%sp-frames d)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; 返回值
;; ----
;; list?
;; 动画帧列表。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-spinner-data '("x" "y") 0 "" "" "" 80 #t 'spinning #f #f)))
  (check (%sp-frames d) => '("x" "y"))
) ;let

(check-report)
