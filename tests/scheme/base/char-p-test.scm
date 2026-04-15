(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; char?
;; 判断对象是否为字符的谓词。
;;
;; 语法
;; ----
;; (char? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 任意对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果对象是字符则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 用于检查对象是否为字符类型
;; 2. 能够正确识别各种字符形式：字母、数字、特殊字符等
;; 3. 返回布尔值，便于在条件判断中使用
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。
;; char? 基础测试
(check (char? #\A) => #t)
(check (char? #\a) => #t)
(check (char? #\0) => #t)
(check (char? #\space) => #t)
(check (char? #\!) => #t)
(check (char? 123) => #f)
(check (char? "A") => #f)
(check (char? 'a) => #f)
;; 错误处理测试
(check-catch 'wrong-number-of-args (char?))
(check-catch 'wrong-number-of-args (char? #\A #\B))
(check-report)