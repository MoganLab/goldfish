(import (liii check))
(import (liii base))

(check-set-mode! 'report-failed)

;; eval-string
;; 将字符串作为 Scheme 代码进行求值。
;;
;; 语法
;; ----
;; (eval-string str)
;; (eval-string str env)
;;
;; 参数
;; ----
;; str : string?
;; 包含 Scheme 代码的字符串。
;;
;; env : let?，可选，默认为 (rootlet)
;; 要在其中求值代码的环境。
;;
;; 返回值
;; ------
;; 字符串中最后一个表达式的求值结果。

;; 测试基本表达式
(check (eval-string "42") => 42)
(check (eval-string "(+ 1 2 3)") => 6)

;; 测试字符串
(check (eval-string "\"hello\"") => "hello")

;; 测试符号
(check (eval-string "'symbol") => 'symbol)

;; 测试列表
(check (eval-string "'(1 2 3)") => '(1 2 3))

;; 测试 lambda 和调用
(check (eval-string "((lambda (x) (* x x)) 5)") => 25)

;; 测试复杂表达式
(check (eval-string "(let ((a 3) (b 4)) (+ a b))") => 7)

;; 测试 if 表达式
(check (eval-string "(if #t 1 2)") => 1)
(check (eval-string "(if #f 1 2)") => 2)

;; 测试比较操作
(check (eval-string "(> 5 3)") => #t)
(check (eval-string "(< 5 3)") => #f)

(check-report)
