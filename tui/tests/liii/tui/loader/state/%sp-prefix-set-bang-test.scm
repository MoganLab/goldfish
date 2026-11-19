;; %sp-prefix-set!
;; 设置微调器内部记录的前缀文本。
;;
;; 语法
;; ----
;; (%sp-prefix-set! d prefix)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; prefix : string?
;; 前缀文本。
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

(let ((d (%make-spinner-data '("a") 0 "" "old" "" 80 #t 'spinning #f #f)))
  (%sp-prefix-set! d "new")
  (check (%sp-prefix d) => "new")
) ;let

(check-report)
