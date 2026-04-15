(import (liii check) (srfi srfi-13))

;; string-suffix?
;; 检查字符串是否以指定后缀结束。
;;
;; 语法
;; ----
;; (string-suffix? suffix str)
;;
;; 参数
;; ----
;; suffix : string?
;; 要检查的后缀字符串。
;;
;; str : string?
;; 要检查的源字符串。
;;
;; 返回值
;; ----
;; boolean
;; 如果str以suffix结尾则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 字符串后缀匹配是指检查指定的后缀字符串是否与源字符串的末尾完全一致。
;; 符合SRFI-13标准规范的字符串后缀检查功能。
;;
;; 建议使用 `string-ends?` 函数代替 `string-suffix?`。
;; `string-ends?` 提供更友好的函数签名（源字符串在前，后缀在后）：
;;   (string-ends? "goldfish" "fish")  ; 直观
;;   (string-suffix? "fish" "goldfish")  ; 参数顺序相反
;; 使用 `gf doc liii/string "string-ends?"` 查看其文档和用例。
;;
;; 空字符串作为suffix时总是返回#t，因为任何字符串都以空字符串结束。
;; 当suffix长度大于源字符串长度时，string-suffix?返回#f。
;; 该函数区分大小写，"Test"不会匹配"test"作为后缀。
;;
;; string-suffix?支持Unicode多字节字符，包括中文、日文、emoji等Unicode字符。
;; 对于多字节字符，操作按字符逻辑进行而非字节级操作，确保Unicode字符被正确处理。
;;
;; 错误处理
;; ----
;; type-error 当任一参数不是字符串类型时抛出。

(check (string-suffix? "" "hello")
  =>
  #t
) ;check
(check (string-suffix? "o" "hello")
  =>
  #t
) ;check
(check (string-suffix? "lo" "hello")
  =>
  #t
) ;check
(check (string-suffix? "llo" "hello")
  =>
  #t
) ;check
(check (string-suffix? "ello" "hello")
  =>
  #t
) ;check
(check (string-suffix? "hello" "hello")
  =>
  #t
) ;check
(check (string-suffix? "123" "test123")
  =>
  #t
) ;check
(check (string-suffix? "" "") => #t)
(check (string-suffix? "a" "a") => #t)
(check (string-suffix? "abc" "abc")
  =>
  #t
) ;check

(check (string-suffix? "b" "ab") => #t)
(check (string-suffix? "" "a") => #t)
(check (string-suffix? "" "") => #t)
(check (string-suffix? "a" "a") => #t)
(check (string-suffix? "ab" "a") => #f)
(check (string-suffix? "short-right"
         "long-suffix-long"
       ) ;string-suffix?
  =>
  #f
) ;check

(check (string-suffix? "文" "中文")
  =>
  #t
) ;check
(check (string-suffix? "测试" "中文测试")
  =>
  #t
) ;check
(check (string-suffix? "code" "unicode")
  =>
  #t
) ;check
(check (string-suffix? "🎉" "🌟🎉")
  =>
  #t
) ;check
(check (string-suffix? "123abc" "中文123abc")
  =>
  #t
) ;check
(check (string-suffix? "边界处理"
         "测试多功能边界处理"
       ) ;string-suffix?
  =>
  #t
) ;check

(check (string-suffix? "hello" "hello")
  =>
  #t
) ;check
(check (string-suffix? "world" "world")
  =>
  #t
) ;check
(check (string-suffix? "完整测试"
         "完整测试"
       ) ;string-suffix?
  =>
  #t
) ;check

(check (string-suffix? "" "") => #t)
(check (string-suffix? "a" "") => #f)
(check (string-suffix? "hello" "")
  =>
  #f
) ;check

(check (string-suffix? "longer-than-original"
         "short"
       ) ;string-suffix?
  =>
  #f
) ;check
(check (string-suffix? "versity" "university")
  =>
  #t
) ;check
(check (string-suffix? "ing" "testing")
  =>
  #t
) ;check

(check (string-suffix? "Test" "hello Test")
  =>
  #t
) ;check
(check (string-suffix? "test" "hello Test")
  =>
  #f
) ;check
(check (string-suffix? "TEST" "test")
  =>
  #f
) ;check
(check (string-suffix? "大写"
         "测试中文字符大写"
       ) ;string-suffix?
  =>
  #t
) ;check
(check (string-suffix? "小" "全部字符小")
  =>
  #t
) ;check

(check (string-suffix? "_file" "_hidden_file")
  =>
  #t
) ;check
(check (string-suffix? "/path" "filedir/path")
  =>
  #t
) ;check
(check (string-suffix? " multiple"
         "with multiple spaces multiple"
       ) ;string-suffix?
  =>
  #t
) ;check

(check (string-suffix? "" "single-char")
  =>
  #t
) ;check
(check (string-suffix? "🙂" "🙂")
  =>
  #t
) ;check
(check (string-suffix? "b⚡c" "testb⚡c")
  =>
  #t
) ;check

(check (string-suffix? ".txt" "document.txt")
  =>
  #t
) ;check
(check (string-suffix? ".json" "data.json")
  =>
  #t
) ;check
(check (string-suffix? ".tmu" "report.tmu")
  =>
  #t
) ;check
(check (string-suffix? "backup.txt"
         "file.backup.txt"
       ) ;string-suffix?
  =>
  #t
) ;check

(check-catch 'wrong-type-arg
  (string-suffix? 123 "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-suffix? "hello" 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-suffix? '(a b c) "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-suffix? "hello" #\c)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-suffix? "hello" 'symbol)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-suffix? '() "hello")
) ;check-catch

(check-report)
