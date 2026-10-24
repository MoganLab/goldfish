(import (liii check)
        (srfi srfi-166 base))

(check-set-mode! 'report-failed)

;; show
;; 组合子格式化的主运行函数，将一系列格式化组合子输出到目标端口或字符串。
;;
;; 语法
;; ----
;; (show out [args ...])
;;
;; 参数
;; ----
;; out : output-port? 或 boolean?
;; - #f : 格式化结果以字符串形式返回
;; - #t : 格式化结果直接输出到当前标准输出端口 (current-output-port)
;; - 端口 : 格式化结果输出到指定的输出端口
;;
;; args : 组合子过程、字符串或其他对象
;;
;; 示例
;; ----
;; (show #f "hi") => "hi"
;; (show #f "hello " "world") => "hello world"
;;
;; 另见
;; ----
;; (liii format base) 同名再导出本函数，日常使用更推荐导入该库：
;;   (import (liii format base))
;;   (show #f "hello " "world")

(check (show #f "hi") => "hi")
(check (show #f "hello " "world") => "hello world")
(check (show #f "a" nothing "b") => "ab")

(check-report)
