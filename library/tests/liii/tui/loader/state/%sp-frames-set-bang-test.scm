;; %sp-frames-set!
;; 设置微调器内部状态记录的帧列表。
;;
;; 语法
;; ----
;; (%sp-frames-set! d frames)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; frames : list?
;; 新的动画帧列表。
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
  (%sp-frames-set! d '("1" "2"))
  (check (%sp-frames d) => '("1" "2"))
) ;let

(check-report)
