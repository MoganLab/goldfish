(import (liii check)
        (liii goldfmt-tokenize))

(check-set-mode! 'report-failed)

;; 测试 escape-string-content：空字符串
(check (escape-string-content "") => "")

;; 测试 escape-string-content：普通字符串（无需转义）
(check (escape-string-content "普通字符串") => "普通字符串")
(check (escape-string-content "hello world") => "hello world")

;; 测试 escape-string-content：包含双引号
(check (escape-string-content "包含\"引号\"") => "包含\\\"引号\\\"")
(check (escape-string-content "\"") => "\\\"")

;; 测试 escape-string-content：包含反斜杠
(check (escape-string-content "包含\\反斜杠") => "包含\\\\反斜杠")
(check (escape-string-content "\\") => "\\\\")

;; 测试 escape-string-content：包含换行符
(check (escape-string-content "第一行\n第二行") => "第一行\\n第二行")
(check (escape-string-content "\n") => "\\n")

;; 测试 escape-string-content：包含回车符（应该被移除）
(check (escape-string-content "包含\r回车") => "包含回车")
(check (escape-string-content "\r") => "")

;; 测试 escape-string-content：混合特殊字符
(check (escape-string-content "\\\"\n") => "\\\\\\\"\\n")

;; 测试 escape-string-content：连续的转义字符
(check (escape-string-content "\\\\\\") => "\\\\\\\\\\\\")
(check (escape-string-content "\"\"\"") => "\\\"\\\"\\\"")

;; 测试 escape-string-content：Scheme 代码片段
(check (escape-string-content "(define x \"hello\")") 
       => "(define x \\\"hello\\\")")

;; 测试 escape-string-content：多行注释内容
(check (escape-string-content "第一行\n第二行\n第三行")
       => "第一行\\n第二行\\n第三行")

(check-report)
