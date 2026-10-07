(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; case
;; case 是 R7RS 定义的多分支条件选择表达式，根据 key 值匹配不同的子句执行。
;;
;; 说明
;; ----
;; case 是 R7RS 定义的分支表达式。
;;
;; 局限性
;; ----
;; case 使用 eqv? 进行匹配，只能精确比较值，不支持复杂的数据解构和谓词匹配。
;; 如需更强大的模式匹配能力，可使用 (liii match) 库：
;;   gf doc liii/match
;;
;; 语法
;; ----
;; (case key clause ...)
;;
;; 参数
;; ----
;; key : any?
;; clause : case 子句
;;
;; 返回值
;; ----
;; any?
;; 返回与 key 匹配的子句结果；未命中时返回未指定值。
;;
;; 注意
;; ----
;; 本文件保留原聚合测试中的符号匹配场景。
;;
;; 示例
;; ----
;; (case '+ ((+ -) 'p0) ((* /) 'p1)) => 'p0
;;
;; 错误处理
;; ----
;; 按命中子句中表达式自身规则处理
(check (case '+ ((+ -) 'p0) ((* /) 'p1)) => 'p0)
(check (case '- ((+ -) 'p0) ((* /) 'p1)) => 'p0)
(check (case '* ((+ -) 'p0) ((* /) 'p1)) => 'p1)
(check (case '@ ((+ -) 'p0) ((* /) 'p1)) => #<unspecified>)
(check (case '& ((+ -) 'p0) ((* /) 'p1)) => #<unspecified>)
(check-report)
