;; repair-report-edits
;; 读取 report 中修复产生的 edit 列表。
;;
;; 语法
;; ----
;; (repair-report-edits report)
;;
;; 参数
;; ----
;; report : repair-report? report 记录
;;
;; 返回值
;; ------
;; list? 修复产生的 edit 列表
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的 edit 列表
(let* ((edit (make-fix-edit :kind 'insert :text ")"))
       (report (make-repair-report :edits (list edit)))
      ) ;
  (check (repair-report-edits report) => (list edit))
) ;let*

;; 测试：缺省值为空列表
(check (repair-report-edits (make-repair-report)) => '())

(check-report)
