;; repair-report-diagnostics
;; 读取 report 中的诊断信息列表。
;;
;; 语法
;; ----
;; (repair-report-diagnostics report)
;;
;; 参数
;; ----
;; report : repair-report? report 记录
;;
;; 返回值
;; ------
;; list? 诊断信息列表
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的诊断信息列表
(let ((report (make-repair-report :diagnostics '((no-insert-position 3)))))
  (check (repair-report-diagnostics report) => '((no-insert-position 3)))
) ;let

;; 测试：缺省值为空列表
(check (repair-report-diagnostics (make-repair-report)) => '())

(check-report)
