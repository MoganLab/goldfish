(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string?
;; 判断给定的对象是否为字符串类型。
;;
;; 语法
;; ----
;; (string? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象
;;
;; 返回值
;; ------
;; boolean?
;; 如果obj是字符串类型则返回#t，否则返回#f
;;
;; 说明
;; ----
;; 1. 用于检查对象是否为字符串类型
;; 2. 能够正确识别空字符串和非空字符串
;; 3. 返回布尔值，便于在条件判断中使用
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。
;; string? 基本测试
(check (string? "hello") => #t)
(check (string? "") => #t)
(check (string? "世界") => #t)
(check (string? "123") => #t)
(check-true (string? "MathAgape"))
;; 非字符串类型测试
(check-false (string? 'a-symbol))
(check-false (string? 123))
(check-false (string? #t))
(check-false (string? #f))
(check-false (string? '()))
(check-false (string? '(1 2 3)))
(check-false (string? #(1 2 3)))
(check-false (string? 3.14))
;; 边界情况测试
(check (string? "\n") => #t)
(check (string? "\t") => #t)
(check (string? " ") => #t)
;; 特殊字符测试
(check (string? "$$$") => #t)
(check (string? "中国") => #t)
(check-report)
