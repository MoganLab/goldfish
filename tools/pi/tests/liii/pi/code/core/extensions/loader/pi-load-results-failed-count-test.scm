;; pi-load-results-failed-count
;; 读取扩展加载报告中的失败数量。
;;
;; 语法
;; ----
;; (pi-load-results-failed-count report)
;;
;; 参数
;; ----
;; report : list?
;; 扩展加载报告 alist。
;;
;; 返回值
;; ----
;; integer
;; 加载失败的扩展数量，报告缺字段时返回 0。

(import (scheme base)
        (liii check)
        (liii pi code core extensions loader)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(check (pi-load-results-failed-count '((total . 3) (success . 2) (failed . 1))) => 1)
(check (pi-load-results-failed-count '((total . 3))) => 0)
(check (pi-load-results-failed-count '()) => 0)

(check-report)
