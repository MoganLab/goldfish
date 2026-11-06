;; %make-spinner-data
;; 创建微调器内部状态记录实例。
;;
;; 语法
;; ----
;; (%make-spinner-data frames frame-idx text prefix suffix interval visible status on-change comp)
;;
;; 参数
;; ----
;; frames : list?
;; 动画帧列表。
;;
;; frame-idx : integer?
;; 当前帧索引。
;;
;; text : string?
;; 提示文本。
;;
;; prefix : string?
;; 前缀文本。
;;
;; suffix : string?
;; 后缀文本。
;;
;; interval : integer?
;; 刷新间隔。
;;
;; visible : boolean?
;; 是否可见。
;;
;; status : symbol?
;; 状态符号。
;;
;; on-change : procedure? 或 #f
;; 变更回调。
;;
;; comp : component? 或 #f
;; 所属组件。
;;
;; 返回值
;; ----
;; %spinner-data?
;; 内部微调器状态记录。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-spinner-data '("a") 0 "txt" "pre" "suf" 80 #t 'spinning #f #f)))
  (check-true (%spinner-data? d))
) ;let

(check-report)
