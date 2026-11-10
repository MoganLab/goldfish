;; %sp-suffix
;; 获取微调器内部记录的后缀文本。
;;
;; 语法
;; ----
;; (%sp-suffix d)
;;
;; 参数
;; ----
;; d : %spinner-data?
;; 内部微调器状态记录。
;;
;; 返回值
;; ----
;; string?
;; 后缀文本。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-spinner-data '("a") 0 "" "" "suf" 80 #t 'spinning #f #f)))
  (check (%sp-suffix d) => "suf")
) ;let

(check-report)
