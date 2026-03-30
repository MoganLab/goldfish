(import (liii check)
        (liii string)
) ;import

;; string-split
;; 按指定字符串分隔符精确分割字符串，保留空字段。
;;
;; 语法
;; ----
;; (string-split str sep)
;;
;; 参数
;; ----
;; str : string?
;; 要分割的源字符串。
;;
;; sep : string? 或 char?
;; 分隔符。支持字符串分隔符，也接受单个字符作为方便写法。
;;
;; 返回值
;; ----
;; list
;; 返回字符串列表，包含所有被 sep 分隔出来的片段。
;;
;; 注意
;; ----
;; - `string-split` 与 `string-tokenize` 不同，它不会压缩连续分隔符。
;; - 当出现连续分隔符、前导分隔符、尾随分隔符时，会保留空字符串。
;; - 当 `sep` 是空字符串时，按字符拆分，返回每个字符对应的单字符串列表。
;; - 当 `str` 为空字符串且 `sep` 非空时，返回 `((""))。
;;
;; 错误处理
;; ----
;; type-error 当 `str` 不是字符串时
;; type-error 当 `sep` 不是字符串或字符时
;; wrong-number-of-args 当参数数量不正确时

; 基本功能测试
(check (string-split "a,b,c" ",") => '(
"a" "b" "c"))
(check (string-split "path::to::file" "::") => '(
"path" "to" "file"))
(check (string-split "2026-03-27" "-") => '(
"2026" "03" "27"))

; 保留空字段
(check (string-split "a,,b," ",") => '(
"a" "" "b" ""))
(check (string-split ",a,b" ",") => '(
"" "a" "b"))
(check (string-split "::a::" "::") => '(
"" "a" ""))

; 未命中与空字符串
(check (string-split "abc" ",") => '(
"abc"))
(check (string-split "" ",") => '(
""))

; 空分隔符按字符拆分
(check (string-split "abc" "") => '(
"a" "b" "c"))
(check (string-split "中文" "") => '(
"中" "文"))
(check (string-split "" "") => '())

; 兼容字符分隔符
(check (string-split "1,2,3" #\,) => '(
"1" "2" "3"))
(check (string-split "line1\nline2\n" #\newline) => '(
"line1" "line2" ""))

; Unicode 与常见 AI Coding 场景
(check (string-split "你好，世界，Goldfish" "，") => '(
"你好" "世界" "Goldfish"))
(check (string-split "name=goldfish&lang=scheme" "&") => '(
"name=goldfish" "lang=scheme"))

; === 以下测试用例与 Python str.split() 保持一致 ===

; 单字符字符串
(check (string-split "a" ",") => '(
"a"))
(check (string-split "x" "x") => '(
"" ""))

; 多字符分隔符边界情况
(check (string-split "abc" "bc") => '(
"a" ""))
(check (string-split "abc" "abc") => '(
"" ""))
(check (string-split "hello world" " world") => '(
"hello" ""))
(check (string-split "a--b--c" "--") => '(
"a" "b" "c"))

; 更多连续分隔符场景
(check (string-split "a,,,b" ",") => '(
"a" "" "" "b"))
(check (string-split ",," ",") => '(
"" "" ""))

; 分隔符重复出现（重叠匹配）- Python 不会重叠匹配
(check (string-split "aaa" "a") => '(
"" "" "" ""))
(check (string-split "aba" "a") => '(
"" "b" ""))
(check (string-split "aaaa" "aa") => '(
"" "" ""))
(check (string-split "aaa" "aa") => '(
"" "a"))

; 更多特殊字符场景
(check (string-split "a\tb\t" "\t") => '(
"a" "b" ""))
(check (string-split "a\nb" "\n") => '(
"a" "b"))
(check (string-split "line1\nline2" "\n") => '(
"line1" "line2"))

; 路径/URL 场景
(check (string-split "/usr/local/bin" "/") => '(
"" "usr" "local" "bin"))
(check (string-split "key=val;key2=val2" ";") => '(
"key=val" "key2=val2"))
(check (string-split "file.txt" ".") => '(
"file" "txt"))
(check (string-split ".hidden" ".") => '(
"" "hidden"))
(check (string-split "." ".") => '(
"" ""))

; 错误处理测试
(check-catch 'type-error (string-split 123 ","))
(check-catch 'type-error (string-split "abc" 123))
(check-catch 'wrong-number-of-args (string-split))
(check-catch 'wrong-number-of-args (string-split "abc"))
(check-catch 'wrong-number-of-args (string-split "abc" "," "extra"))

(check-report)
