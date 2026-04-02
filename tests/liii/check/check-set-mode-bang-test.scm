(import (liii check)
        (rename (srfi srfi-78)
                (check-reset! srfi-78-check-reset!)
        ) ;rename
) ;import

(check-set-mode! 'report-failed)

;; check-set-mode!
;; 设置测试输出与报告模式。
;;
;; 语法
;; ----
;; (check-set-mode! mode)
;;
;; 参数
;; ----
;; mode : symbol?
;; 支持 `'off`、`'summary`、`'report-failed`、`'report`。

(check-set-mode! 'off)
(check:proc 'ignored-in-off-mode
            (lambda () 1)
            2)
(check-false (check-failed?))

(srfi-78-check-reset!)

(check-set-mode! 'summary)
(check (+ 1 2) => 3)

(check-set-mode! 'report)
(check (list 'a 'b) => '(a b))

(check-set-mode! 'report-failed)
(check-true #t)

(check-report)
