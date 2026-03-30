(import (liii check)
        (liii string)
) ;import

;; string-join
;; 将一个字符串列表通过指定的分隔符连接起来。
;;
;; 语法
;; ----
;; (string-join string-list)
;; (string-join string-list delimiter)
;; (string-join string-list delimiter grammar)
;;
;; 参数
;; ----
;; string-list : list
;; 一个字符串列表，可以包含零个或多个字符串元素。
;; * 空列表时的行为：所有模式返回空字符串，仅'strict-infix模式会抛出异常
;; * 元素要求：每个元素必须是字符串类型，否则会抛出type-error异常
;;
;; delimiter : string
;; 用作分隔符的字符串，默认值为空字符串""（等价于不使用分隔符）。
;; * 支持任意字符串作为分隔符，包括空字符串、中文、emoji、转义字符等
;;
;; grammar : symbol
;; 指定连接语法模式，可选值包括：
;; - 'infix（或省略）：在中缀模式下，分隔符放在每对相邻元素之间
;; - 'suffix：在后缀模式下，分隔符放在每个元素（包括最后一个）之后
;; - 'prefix：在前缀模式下，分隔符放在每个元素（包括第一个）之前
;; - 'strict-infix：严格中缀模式，要求string-list不能为空，否则会抛错
;; * 严格中缀模式('strict-infix)是唯一对空列表会抛出异常的模式
;;
;; 返回值
;; ----
;; string
;; 返回由string-list中的字符串按指定语法模式连接而成的字符串。
;;
;; 错误处理
;; ----
;; value-error 当语法模式为'strict-infix且string-list为空列表时
;; value-error 当提供了无效的语法模式时
;; type-error  当提供了无效的参数类型时
;; wrong-number-of-args 当参数数量不正确时

;; 基本功能测试
(check (string-join '("a" "b" "c")) => "abc")
(check (string-join '("a" "b" "c") ":") => "a:b:c")
(check (string-join '("a" "b" "c") ":" 'infix) => "a:b:c")
(check (string-join '("a" "b" "c") ":" 'suffix) => "a:b:c:")
(check (string-join '("a" "b" "c") ":" 'prefix) => ":a:b:c")

;; 空列表测试
(check (string-join '() ":") => "")
(check (string-join '() ":" 'infix) => "")
(check (string-join '() ":" 'prefix) => "")
(check (string-join '() ":" 'suffix) => "")

;; strict-infix 模式错误测试
(check-catch 'value-error (string-join '() ":" 'strict-infix))
(check-catch 'value-error (string-join '() "" 'strict-infix))
(check-catch 'value-error (string-join '() "分隔" 'strict-infix))

;; 无效参数类型测试
(check-catch 'type-error (string-join '() ":" 2))
(check-catch 'value-error (string-join '() ":" 'no-such-grammer))
(check-catch 'wrong-number-of-args (string-join '() ":" 1 2 3))

;; 空字符串元素边界测试
(check (string-join '("" "") ":") => ":")
(check (string-join '("" "" "") ":") => "::")
(check (string-join '("" "") "") => "")
(check (string-join '("" "" "") "") => "")
(check (string-join '("" "") "分隔符") => "分隔符")
(check (string-join '("" "" "") "分隔符") => "分隔符分隔符")
(check (string-join '("" "") "" 'suffix) => "")
(check (string-join '("" "") "" 'prefix) => "")
(check (string-join '("" "" "") "分隔" 'suffix) => "分隔分隔分隔")
(check (string-join '("元素1" "元素2" "元素3") "" 'prefix) => "元素1元素2元素3")

;; 中文和Unicode字符边界测试
(check (string-join '("中文" "测试" "字符串")) => "中文测试字符串")
(check (string-join '("中文" "测试" "字符串") "间") => "中文间测试间字符串")
(check (string-join '("中文1" "中文2" "中文3") "分隔") => "中文1分隔中文2分隔中文3")

;; emoji和特殊字符边界测试
(check (string-join '("🌟" "🎉" "😀") "-") => "🌟-🎉-😀")
(check (string-join '("🌟" "🎉" "😀") "🎯") => "🌟🎯🎉🎯😀")
(check (string-join '("hello" "test") ":") => "hello:test")

;; 单元素边界测试
(check (string-join '("单元素测试") ",") => "单元素测试")

(check-report)
