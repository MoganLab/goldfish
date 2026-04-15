(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string-length
;; 返回给定字符串在UTF-8编码下的字节长度。
;;
;; 语法
;; ----
;; (string-length string)
;;
;; 参数
;; ----
;; string : string?
;; 要测量长度的字符串，可以是空字符串、单字符字符串或多字符字符串。
;;
;; 返回值
;; ------
;; integer?
;; 返回一个非负整数，表示字符串在UTF-8编码下的字节长度。
;;
;; 说明
;; ----
;; 1. 字符串长度计算包含所有字节编码单元，包括空格、制表符和换行符
;; 2. 空字符串 "" 的长度为 0
;; 3. 对于ASCII字符（0-127），每个字符占用1字节
;; 4. 对于非ASCII字符，每个字符可能占用2-4字节UTF-8编码单元
;; 5. 字符串不会改变原始数据，只是返回长度信息
;;
;; 边界情况
;; --------
;; - 空字符串长度：0
;; - ASCII字符长度：1字节/字符
;; - UTF-8非ASCII字符：2-4字节/字符
;; - 多字节编码字符串：总字节长度
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是字符串类型时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1个时抛出错误。
;; string-length 基础测试
(check (string-length "") => 0)
(check (string-length "a") => 1)
(check (string-length "hello") => 5)
(check (string-length "世界") => 6)
(check (string-length "你好世界")
  =>
  12
) ;check
;; 空字符串测试
(check (string-length "") => 0)
(check (string-length (list->string '()))
  =>
  0
) ;check
;; 单字符测试
(check (string-length "a") => 1)
(check (string-length "A") => 1)
(check (string-length "1") => 1)
(check (string-length "!") => 1)
(check (string-length " ") => 1)
;; 多字符测试
(check (string-length "abc") => 3)
(check (string-length "ABC") => 3)
(check (string-length "123") => 3)
(check (string-length "!@#") => 3)
;; 含有空格的字符串
(check (string-length "hello world")
  =>
  11
) ;check
(check (string-length "  ") => 2)
(check (string-length " leading space")
  =>
  14
) ;check
(check (string-length "trailing space ")
  =>
  15
) ;check
;; 特殊字符和空白字符
(check (string-length "hello\nworld")
  =>
  11
) ;check
(check (string-length "tab\tseparated")
  =>
  13
) ;check
(check (string-length "line\rreturn")
  =>
  11
) ;check
;; Unicode字符测试 - 按字节编码单元计数
(check (string-length "😀") => 4)
(check (string-length "μ") => 2)
(check (string-length "ä") => 2)
(check (string-length "中文") => 6)
;; 长度边界测试
(check (string-length "a") => 1)
(check (string-length "abcdefghijklmnop")
  =>
  16
) ;check
(check (string-length "abcdefghijklmnopqrstuvwxyz"
       ) ;string-length
  =>
  26
) ;check
(check (string-length "aaaaaaaaaaaaaaaaaaaaaaaaaa"
       ) ;string-length
  =>
  26
) ;check
;; 与字符串生成函数的兼容性测试
(check (string-length (make-string 5 #\a))
  =>
  5
) ;check
(check (string-length (make-string 10 #\x))
  =>
  10
) ;check
(check (string-length (make-string 0))
  =>
  0
) ;check
;; 与字符串拼接函数的兼容性测试
(check (string-length (string-append "hello" "world")
       ) ;string-length
  =>
  10
) ;check
(check (string-length (string-append "" ""))
  =>
  0
) ;check
(check (string-length (string-append "a" "b"))
  =>
  2
) ;check
;; 错误处理测试
(check-catch 'wrong-type-arg
  (string-length 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-length 'symbol)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-length #t)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-length '())
) ;check-catch
(check-catch 'wrong-type-arg
  (string-length #(1 2 3))
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-length)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-length "hello" "world")
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-length "hello" 1)
) ;check-catch
(check-report)
