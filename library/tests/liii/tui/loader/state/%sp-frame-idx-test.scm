;; %sp-frame-idx
;; 获取微调器内部记录的当前帧索引。
;;
;; 语法
;; ----
;; (%sp-frame-idx d)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; 返回值
;; ----
;; integer?
;; 当前帧索引。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-spinner-data '("a") 3 "" "" "" 80 #t 'spinning #f #f)))
  (check (%sp-frame-idx d) => 3)
) ;let

(check-report)
