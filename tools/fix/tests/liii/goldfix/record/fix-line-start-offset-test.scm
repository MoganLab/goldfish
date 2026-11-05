;; fix-line-start-offset
;; 读取行记录在源码中的起始偏移量。
;;
;; 语法
;; ----
;; (fix-line-start-offset line)
;;
;; 参数
;; ----
;; line : fix-line? 行记录
;;
;; 返回值
;; ------
;; integer? 该行在源码中的起始偏移量
(import (liii check) (liii base) (liii goldfix record))

(check-set-mode! 'report-failed)


;; 测试：读取创建时指定的起始偏移量
(check (fix-line-start-offset (make-fix-line :start-offset 12)) => 12)

;; 测试：缺省起始偏移量为 0
(check (fix-line-start-offset (make-fix-line)) => 0)

(check-report)
