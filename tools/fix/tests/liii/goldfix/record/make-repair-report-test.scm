;; make-repair-report
;; 创建一条汇总括号修复结果的 report 记录。
;;
;; 语法
;; ----
;; (make-repair-report (ok? #f) (edits '()) (diagnostics '()))
;;
;; 参数
;; ----
;; ok? : boolean? 修复后括号是否平衡
;; edits : list? 修复产生的 edit 列表
;; diagnostics : list? 修复过程中的诊断信息列表
;;
;; 返回值
;; ------
;; repair-report 记录对象
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：创建 report 并读取各字段
(let* ((edit (make-fix-edit :kind 'insert :text ")"))
       (report (make-repair-report :ok? #t :edits (list edit) :diagnostics '()))
      ) ;
  (check (repair-report? report) => #t)
  (check (repair-report-ok? report) => #t)
  (check (repair-report-edits report) => (list edit))
  (check (repair-report-diagnostics report) => '())
) ;let*

;; 测试：缺省参数
(let ((report (make-repair-report)))
  (check (repair-report-ok? report) => #f)
  (check (repair-report-edits report) => '())
  (check (repair-report-diagnostics report) => '())
) ;let

;; 测试：ok? 不是布尔值时报错
(check-catch 'value-error (make-repair-report :ok? 'yes))

;; 测试：edits 不是列表时报错
(check-catch 'value-error (make-repair-report :edits 'x))

;; 测试：diagnostics 不是列表时报错
(check-catch 'value-error (make-repair-report :diagnostics 'x))

(check-report)
