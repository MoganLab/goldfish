(import (liii check) (liii string))

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

(check (string-split "a,b,c" ",")
  =>
  '("a" "b" "c")
) ;check
(check (string-split "path::to::file" "::")
  =>
  '("path" "to" "file")
) ;check
(check (string-split "2026-03-27" "-")
  =>
  '("2026" "03" "27")
) ;check

(check (string-split "a,,b," ",")
  =>
  '("a" "" "b" "")
) ;check
(check (string-split ",a,b" ",")
  =>
  '("" "a" "b")
) ;check
(check (string-split "::a::" "::")
  =>
  '("" "a" "")
) ;check

(check (string-split "abc" ",")
  =>
  '("abc")
) ;check
(check (string-split "" ",") => '(""))

(check (string-split "abc" "")
  =>
  '("a" "b" "c")
) ;check
(check (string-split "中文" "")
  =>
  '("中" "文")
) ;check
(check (string-split "" "") => '())

(check (string-split "1,2,3" #\,)
  =>
  '("1" "2" "3")
) ;check
(check (string-split "line1\nline2\n"
         #\newline
       ) ;string-split
  =>
  '("line1" "line2" "")
) ;check

(check (string-split "你好，世界，Goldfish"
         "，"
       ) ;string-split
  =>
  '("你好" "世界" "Goldfish")
) ;check
(check (string-split "name=goldfish&lang=scheme"
         "&"
       ) ;string-split
  =>
  '("name=goldfish" "lang=scheme")
) ;check


(check (string-split "a" ",") => '("a"))
(check (string-split "x" "x")
  =>
  '("" "")
) ;check

(check (string-split "abc" "bc")
  =>
  '("a" "")
) ;check
(check (string-split "abc" "abc")
  =>
  '("" "")
) ;check
(check (string-split "hello world" " world")
  =>
  '("hello" "")
) ;check
(check (string-split "a--b--c" "--")
  =>
  '("a" "b" "c")
) ;check

(check (string-split "a,,,b" ",")
  =>
  '("a" "" "" "b")
) ;check
(check (string-split ",," ",")
  =>
  '("" "" "")
) ;check

(check (string-split "aaa" "a")
  =>
  '("" "" "" "")
) ;check
(check (string-split "aba" "a")
  =>
  '("" "b" "")
) ;check
(check (string-split "aaaa" "aa")
  =>
  '("" "" "")
) ;check
(check (string-split "aaa" "aa")
  =>
  '("" "a")
) ;check

(check (string-split "a\tb\t" "\t")
  =>
  '("a" "b" "")
) ;check
(check (string-split "a\nb" "\n")
  =>
  '("a" "b")
) ;check
(check (string-split "line1\nline2" "\n")
  =>
  '("line1" "line2")
) ;check

(check (string-split "/usr/local/bin" "/")
  =>
  '("" "usr" "local" "bin")
) ;check
(check (string-split "key=val;key2=val2" ";")
  =>
  '("key=val" "key2=val2")
) ;check
(check (string-split "file.txt" ".")
  =>
  '("file" "txt")
) ;check
(check (string-split ".hidden" ".")
  =>
  '("" "hidden")
) ;check
(check (string-split "." ".")
  =>
  '("" "")
) ;check

(check-catch 'type-error
  (string-split 123 ",")
) ;check-catch
(check-catch 'type-error
  (string-split "abc" 123)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-split)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-split "abc")
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-split "abc" "," "extra")
) ;check-catch

(check-report)
