;; pending-close-frame
;; 读取 pending-close 所属的 open-frame 记录。
;;
;; 语法
;; ----
;; (pending-close-frame pending)
;;
;; 参数
;; ----
;; pending : pending-close? pending-close 记录
;;
;; 返回值
;; ------
;; open-frame? 该右括号所属的 open-frame 记录
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的 open-frame
(let* ((frame (make-open-frame :tag-name "begin"))
       (pending (make-pending-close :frame frame))
      ) ;
  (check (pending-close-frame pending) => frame)
) ;let*

(check-report)
