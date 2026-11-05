;; make-open-frame
;; 创建一个仍然打开的左括号环境记录。
;;
;; 语法
;; ----
;; (make-open-frame (offset 0) (line 1) (column 0) (tag-name ""))
;;
;; 参数
;; ----
;; offset : integer? 左括号在源码中的偏移量
;; line : integer? 左括号所在的行号
;; column : integer? 左括号所在的列号
;; tag-name : string? 左括号对应的标记名称
;;
;; 返回值
;; ------
;; open-frame 记录对象
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：创建 open-frame 并读取各字段
(let ((frame (make-open-frame :offset 4 :line 2 :column 2 :tag-name "begin")))
  (check (open-frame? frame) => #t)
  (check (open-frame-offset frame) => 4)
  (check (open-frame-line frame) => 2)
  (check (open-frame-column frame) => 2)
  (check (open-frame-tag-name frame) => "begin")
) ;let

;; 测试：缺省参数
(let ((frame (make-open-frame)))
  (check (open-frame-offset frame) => 0)
  (check (open-frame-line frame) => 1)
  (check (open-frame-column frame) => 0)
  (check (open-frame-tag-name frame) => "")
) ;let

;; 测试：偏移量不是整数时报错
(check-catch 'value-error (make-open-frame :offset "0"))

;; 测试：偏移量为负数时报错
(check-catch 'value-error (make-open-frame :offset -1))

;; 测试：tag-name 不是字符串时报错
(check-catch 'value-error (make-open-frame :tag-name 'begin))

(check-report)
