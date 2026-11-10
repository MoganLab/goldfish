;; make-fix-edit
;; 创建一条描述源码改动的 edit 记录。
;;
;; 语法
;; ----
;; (make-fix-edit (kind 'insert) (offset 0) (start 0) (end 0) (text "") (reason "") (open-offset #f))
;;
;; 参数
;; ----
;; kind : symbol? 编辑类型，insert 表示插入，delete 表示删除
;; offset : integer? 插入位置（kind 为 insert 时生效）
;; start : integer? 删除区间的起始偏移量（kind 为 delete 时生效）
;; end : integer? 删除区间的结束偏移量（kind 为 delete 时生效）
;; text : string? 插入的文本
;; reason : string? 编辑原因，如 eof / extra-close
;; open-offset : integer? 对应左括号的偏移量，未知时为 #f
;;
;; 返回值
;; ------
;; fix-edit 记录对象
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：创建插入型 edit 并读取各字段
(let ((edit (make-fix-edit :kind 'insert :offset 10 :text ")" :reason "eof" :open-offset 0)
      ) ;edit
     ) ;
  (check (fix-edit? edit) => #t)
  (check (fix-edit-kind edit) => 'insert)
  (check (fix-edit-offset edit) => 10)
  (check (fix-edit-text edit) => ")")
  (check (fix-edit-reason edit) => "eof")
  (check (fix-edit-open-offset edit) => 0)
) ;let

;; 测试：创建删除型 edit 并读取各字段
(let ((edit (make-fix-edit
              :kind        'delete
              :start       5
              :end         6
              :reason      "extra-close"
              :open-offset #f
            ) ;make-fix-edit
      ) ;edit
     ) ;
  (check (fix-edit-kind edit) => 'delete)
  (check (fix-edit-start edit) => 5)
  (check (fix-edit-end edit) => 6)
  (check (fix-edit-reason edit) => "extra-close")
  (check (fix-edit-open-offset edit) => #f)
) ;let

;; 测试：缺省参数
(let ((edit (make-fix-edit)))
  (check (fix-edit-kind edit) => 'insert)
  (check (fix-edit-offset edit) => 0)
  (check (fix-edit-start edit) => 0)
  (check (fix-edit-end edit) => 0)
  (check (fix-edit-text edit) => "")
  (check (fix-edit-reason edit) => "")
  (check (fix-edit-open-offset edit) => #f)
) ;let

;; 测试：kind 非法时报错
(check-catch 'value-error (make-fix-edit :kind 'replace))

;; 测试：偏移量为负数时报错
(check-catch 'value-error (make-fix-edit :offset -1))

;; 测试：end 小于 start 时报错
(check-catch 'value-error (make-fix-edit :kind 'delete :start 5 :end 4))

;; 测试：text 不是字符串时报错
(check-catch 'value-error (make-fix-edit :text 'x))

;; 测试：reason 不是字符串时报错
(check-catch 'value-error (make-fix-edit :reason 'x))

;; 测试：open-offset 非法时报错
(check-catch 'value-error (make-fix-edit :open-offset -1))

(check-report)
