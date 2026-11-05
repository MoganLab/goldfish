;; make-pending-close
;; 创建一个等待下一行缩进确认的右括号记录。
;;
;; 语法
;; ----
;; (make-pending-close (frame (make-open-frame)) (offset 0) (end 0) (line 1) (column 0))
;;
;; 参数
;; ----
;; frame : open-frame? 该右括号所属的 open-frame 记录
;; offset : integer? 右括号在源码中的起始偏移量
;; end : integer? 右括号在源码中的结束偏移量
;; line : integer? 右括号所在的行号
;; column : integer? 右括号所在的列号
;;
;; 返回值
;; ------
;; pending-close 记录对象
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：创建 pending-close 并读取各字段
(let* ((frame (make-open-frame :tag-name "begin"))
       (pending (make-pending-close :frame frame :offset 20 :end 21 :line 2 :column 15)
       ) ;pending
      ) ;
  (check (pending-close? pending) => #t)
  (check (pending-close-frame pending) => frame)
  (check (pending-close-offset pending) => 20)
  (check (pending-close-end pending) => 21)
  (check (pending-close-line pending) => 2)
  (check (pending-close-column pending) => 15)
) ;let*

;; 测试：缺省参数
(let ((pending (make-pending-close)))
  (check (pending-close-offset pending) => 0)
  (check (pending-close-end pending) => 0)
  (check (pending-close-line pending) => 1)
  (check (pending-close-column pending) => 0)
  (check (open-frame? (pending-close-frame pending)) => #t)
) ;let

;; 测试：frame 不是 open-frame 时报错
(check-catch 'value-error (make-pending-close :frame 'x))

;; 测试：偏移量为负数时报错
(check-catch 'value-error (make-pending-close :offset -1))

;; 测试：end 小于 offset 时报错
(check-catch 'value-error (make-pending-close :offset 5 :end 4))

(check-report)
