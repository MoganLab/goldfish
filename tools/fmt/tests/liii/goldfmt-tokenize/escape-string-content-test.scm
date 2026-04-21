(import (liii check)
  (liii goldfmt-tokenize)
) ;import

(check-set-mode! 'report-failed)

;; 测试 escape-string-content：空字符串
(check (escape-string-content "") => "")

;; 测试 escape-string-content：普通字符串（无需转义）
(check (escape-string-content "普通字符串"
       ) ;escape-string-content
  =>
  "普通字符串"
) ;check
(check (escape-string-content "hello world")
  =>
  "hello world"
) ;check

;; 测试 escape-string-content：包含双引号
(check (escape-string-content "包含\"引号\""
       ) ;escape-string-content
  =>
  "包含\\\"引号\\\""
) ;check
(check (escape-string-content "\"")
  =>
  "\\\""
) ;check

;; 测试 escape-string-content：包含反斜杠
(check (escape-string-content "包含\\反斜杠"
       ) ;escape-string-content
  =>
  "包含\\\\反斜杠"
) ;check
(check (escape-string-content "\\")
  =>
  "\\\\"
) ;check

;; 测试 escape-string-content：包含换行符
(check (escape-string-content "第一行\n第二行"
       ) ;escape-string-content
  =>
  "第一行\\n第二行"
) ;check
(check (escape-string-content "\n")
  =>
  "\\n"
) ;check

;; 测试 escape-string-content：包含回车符（应该被移除）
(check (escape-string-content "包含\r回车")
  =>
  "包含回车"
) ;check
(check (escape-string-content "\r")
  =>
  ""
) ;check

;; 测试 escape-string-content：混合特殊字符
(check (escape-string-content "\\\"\n")
  =>
  "\\\\\\\"\\n"
) ;check

;; 测试 escape-string-content：连续的转义字符
(check (escape-string-content "\\\\\\")
  =>
  "\\\\\\\\\\\\"
) ;check
(check (escape-string-content "\"\"\"")
  =>
  "\\\"\\\"\\\""
) ;check

;; 测试 escape-string-content：Scheme 代码片段
(check (escape-string-content "(define x \"hello\")"
       ) ;escape-string-content
  =>
  "(define x \\\"hello\\\")"
) ;check

;; 测试 escape-string-content：多行注释内容
(check (escape-string-content "第一行\n第二行\n第三行"
       ) ;escape-string-content
  =>
  "第一行\\n第二行\\n第三行"
) ;check

(check-report)
