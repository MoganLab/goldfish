(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; not
;; 对单个参数执行逻辑非操作，否定其布尔值。
;;
;; 语法
;; ----
;; (not obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。根据R7RS，任何非#f的值都被视为真值。
;;
;; 返回值
;; ------
;; boolean?
;; 如果传入的对象是#f则返回#t，否则返回#f。
;;
;; 说明
;; ----
;; 1. 逻辑否定：将真值转换为假值，将假值转换为真值。
;; 2. 真值判断：任何非#f的值都被视为真值，包括空列表'()、0、空字符串""等。
;; 3. 布尔运算：门用于布尔值的逻辑取反运算。
;;
;; 示例
;; ----
;; (not #t) => #f
;; (not #f) => #t
;; (not 1) => #f
;; (not '()) => #f
;; (not "hello") => #f
;;
;; 错误处理
;; --------
;; 无错误情况。
;; not 基础测试
(check-false (not #t))
(check-true (not #f))
;; not 真值测试
(check-false (not 1))
(check-false (not 0))
(check-false (not '()))
(check-false (not 'a))
(check-false (not "string"))
(check-false (not #\a))
;; not 边界测试
(check-true (not #f))
(check-false (not #t))
;; not 连续取反测试
(check-true (not (not #t)))
(check-false (not (not #f)))
(check-true (not (not #t)))
;; not 与谓词组合测试
(check-true (not (= 3 4)))
(check-false (not (= 3 3)))
;; not 实际用途测试
(check-false (not (list? '(1 2 3))))
(check-true (not (list? 123)))
(check-report)