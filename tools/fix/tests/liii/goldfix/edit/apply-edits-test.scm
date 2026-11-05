;; apply-edits
;; 将一组 edit 应用于原始 source，返回修正后的源码文本。
;; 编辑按位置从后往前应用，因此前面的编辑不会影响后面编辑的原始 offset。
;;
;; 语法
;; ----
;; (apply-edits source edits)
;;
;; 参数
;; ----
;; source : string? 原始源码文本
;; edits : list? edit 记录列表
;;
;; 返回值
;; ------
;; string? 应用全部 edit 之后的源码文本

(import (liii check) (liii goldfix edit) (liii goldfix record))

(check-set-mode! 'report-failed)

;; apply-edits
;; 将 insert/delete edit 应用于原始 source。

(check (apply-edits "(define x 1"
         (list (make-fix-edit :kind 'insert :offset 11 :text ")" :reason "eof" :open-offset 0)
         ) ;list
       ) ;apply-edits
  =>
  "(define x 1)"
) ;check

(check (apply-edits "(define x 1))"
         (list (make-fix-edit
                 :kind        'delete
                 :start       12
                 :end         13
                 :reason      "extra-close"
                 :open-offset #f
               ) ;make-fix-edit
         ) ;list
       ) ;apply-edits
  =>
  "(define x 1)"
) ;check

(check (apply-edits "(begin\n  (display \"x\"))\n  (newline))"
         (list (make-fix-edit
                 :kind        'delete
                 :start       22
                 :end         23
                 :reason      "premature-close"
                 :open-offset 0
               ) ;make-fix-edit
         ) ;list
       ) ;apply-edits
  =>
  "(begin\n  (display \"x\")\n  (newline))"
) ;check

;; offset 从后往前应用，前面的 edit 不会影响后面的原始 offset。

(check (apply-edits "(a b))"
         (list (make-fix-edit
                 :kind        'delete
                 :start       5
                 :end         6
                 :reason      "extra-close"
                 :open-offset #f
               ) ;make-fix-edit
           (make-fix-edit :kind 'insert :offset 0 :text "'" :reason "test" :open-offset #f)
         ) ;list
       ) ;apply-edits
  =>
  "'(a b)"
) ;check

(check-report)
