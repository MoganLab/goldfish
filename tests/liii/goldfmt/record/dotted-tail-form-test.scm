;; dotted-tail-form
;; 获取点尾部记录所指向的 form。
;;
;; 语法
;; ----
;; (dotted-tail-form dotted-tail)
;;
;; 参数
;; ----
;; dotted-tail : dotted-tail?
;; 目标点尾部记录。
;;
;; 返回值
;; ----
;; any?
;; 点尾部所指向的 form。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(check (dotted-tail-form (make-dotted-tail 'rest)) => 'rest)
(check (dotted-tail-form (make-dotted-tail)) => '())

(check-report)
