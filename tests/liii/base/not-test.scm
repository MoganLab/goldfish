(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

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
(check-false (not #t))          ; #t取反应返回#f
(check-true (not #f))           ; #f取反应返回#t

;; not 真值测试
(check-false (not 1))           ; 任何非#f值都视为真值
(check-false (not 0))           ; 0被视为真值
(check-false (not '()))         ; 空列表被视为真值
(check-false (not 'a))          ; 符号被视为真值
(check-false (not "string"))    ; 字符串被视为真值
(check-false (not #\a))         ; 字符被视为真值

;; not 边界测试
(check-true (not #f))           ; 假值验证
(check-false (not #t))          ; 真值验证

;; not 连续取反测试
(check-true (not (not #t)))     ; 双重否定
(check-false (not (not #f)))    ; 双重否定
(check-true (not (not #t)))     ; 再次验证

;; not 与谓词组合测试
(check-true (not (= 3 4)))      ; 假条件
(check-false (not (= 3 3)))     ; 真条件

;; not 实际用途测试
(check-false (not (list? '(1 2 3))))
(check-true (not (list? 123)))

(check-report)
