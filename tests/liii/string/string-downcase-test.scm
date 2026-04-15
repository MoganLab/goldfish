(import (liii check) (liii string))

;; string-downcase
;; 将字符串转换为其小写等价形式。
;;
;; 语法
;; ----
;; (string-downcase str)
;; (string-downcase str start)
;; (string-downcase str start end)
;;
;; 参数
;; ----
;; str : string?
;; 要转换的源字符串。
;;
;; start : integer? 可选
;; 转换的起始位置(包含)，默认为0。
;;
;; end : integer? 可选
;; 转换的结束位置(不包含)，默认为字符串长度。
;;
;; 返回值
;; ----
;; string
;; 返回一个新的字符串，其中str中从start到end的字符被转换为小写形式。
;;
;; 注意
;; ----
;; string-downcase对字符串中每个大写字符的指定范围应用字符映射转换，使用ASCII字符大小写映射。
;; 只有ASCII范围内的字符会被转换，非ASCII字符保持不变。
;;
;; 当前实现**仅支持ASCII字符范围**内的转换（A-Z→a-z）。
;; - 非ASCII字符（如中文、拉丁扩展字符、希腊字母等）保持不变
;; - Unicode复杂字符（如À, Á, Ω等）**不被支持转换**
;;
;; 对于纯ASCII字符串，转换规则很简单：A-Z被映射到a-z。
;; 对于没有ASCII大写字母的字符串，将返回原字符串内容的完整副本。
;;
;; 对于空字符串输入，始终返回空字符串。
;; 对于没有大写字母的字符串，将返回原字符串内容的完整副本。
;;
;; 错误处理
;; ----
;; out-of-range 当start/end超出字符串索引范围时
;; wrong-type-arg 当str不是字符串类型时

;; ; Basic functionality tests for string-downcase
(check (string-downcase "ABC") => "abc")
(check (string-downcase "abc") => "abc")
(check (string-downcase "ABC123")
  =>
  "abc123"
) ;check
(check (string-downcase "123ABC")
  =>
  "123abc"
) ;check
(check (string-downcase "Hello World")
  =>
  "hello world"
) ;check
(check (string-downcase "!@#$%")
  =>
  "!@#$%"
) ;check
(check (string-downcase "MixedCaseString")
  =>
  "mixedcasestring"
) ;check
(check (string-downcase "UPPERCASE")
  =>
  "uppercase"
) ;check
(check (string-downcase "lowercase")
  =>
  "lowercase"
) ;check
(check (string-downcase "CamelCaseISAGoodName")
  =>
  "camelcaseisagoodname"
) ;check
(check (string-downcase "") => "")
(check (string-downcase "A") => "a")
(check (string-downcase "Z") => "z")
(check (string-downcase "a1B2c3D4")
  =>
  "a1b2c3d4"
) ;check

;; ; 验证非ASCII字符保持不变（当前实现只支持基本ASCII）
(check (string-downcase "中文")
  =>
  "中文"
) ;check
(check (string-downcase "中文TEST功能")
  =>
  "中文test功能"
) ;check
(check (string-downcase "ÀÁÂ")
  =>
  "ÀÁÂ"
) ;check
(check (string-downcase "À") => "À")
(check (string-downcase "Á") => "Á")
(check (string-downcase "ÄÖÜ")
  =>
  "ÄÖÜ"
) ;check
(check (string-downcase "ΑΒΓ")
  =>
  "ΑΒΓ"
) ;check
(check (string-downcase "café")
  =>
  "café"
) ;check

;; ; Mixed alphanumeric and special characters
(check (string-downcase "ABC-def-GHI")
  =>
  "abc-def-ghi"
) ;check
(check (string-downcase "123-ABC-xyz")
  =>
  "123-abc-xyz"
) ;check
(check (string-downcase "___ABCDE___")
  =>
  "___abcde___"
) ;check
(check (string-downcase ".COM/.NET/.ORG")
  =>
  ".com/.net/.org"
) ;check

;; ; Edge cases - single character
(check (string-downcase "X") => "x")
(check (string-downcase "x") => "x")
(check (string-downcase "0") => "0")
(check (string-downcase ".") => ".")
(check (string-downcase " ") => " ")

;; ; String case variations
(check (string-downcase "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       ) ;string-downcase
  =>
  "abcdefghijklmnopqrstuvwxyz"
) ;check
(check (string-downcase "abcdefghijklmnopqrstuvwxyz"
       ) ;string-downcase
  =>
  "abcdefghijklmnopqrstuvwxyz"
) ;check
(check (string-downcase "AbCdEfGhIjKlMnOpQrStUvWxYz"
       ) ;string-downcase
  =>
  "abcdefghijklmnopqrstuvwxyz"
) ;check
(check (string-downcase "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
       ) ;string-downcase
  =>
  "0123456789abcdefghijklmnopqrstuvwxyz0123456789"
) ;check

;; ; Boundary conditions
(check (string-downcase "A" 0) => "a")
(check (string-downcase "ABC" 0)
  =>
  "abc"
) ;check
(check (string-downcase "ABC" 0 1)
  =>
  "aBC"
) ;check
(check (string-downcase "ABC" 0 2)
  =>
  "abC"
) ;check
(check (string-downcase "ABC" 0 3)
  =>
  "abc"
) ;check
(check (string-downcase "ABC" 1)
  =>
  "Abc"
) ;check
(check (string-downcase "ABC" 1 2)
  =>
  "AbC"
) ;check
(check (string-downcase "ABC" 1 3)
  =>
  "Abc"
) ;check
(check (string-downcase "ABC" 2)
  =>
  "ABc"
) ;check
(check (string-downcase "ABC" 2 3)
  =>
  "ABc"
) ;check

;; ; ASCII boundary verification
(check (string-downcase "aBc" 0 1)
  =>
  "aBc"
) ;check
(check (string-downcase "aBc" 0 0)
  =>
  "aBc"
) ;check

;; Additional boundary tests
(check (string-downcase "A1B2C3D4E5" 0)
  =>
  "a1b2c3d4e5"
) ;check
(check (string-downcase "A1B2C3D4E5" 2)
  =>
  "A1b2c3d4e5"
) ;check
(check (string-downcase "A1B2C3D4E5" 2 5)
  =>
  "A1b2c3D4E5"
) ;check
(check (string-downcase "A1B2C3D4E5" 0 1)
  =>
  "a1B2C3D4E5"
) ;check

;; ; Spanning over different character types
(check (string-downcase "TEST123" 0 4)
  =>
  "test123"
) ;check
(check (string-downcase "TEST123" 2 5)
  =>
  "TEst123"
) ;check
(check (string-downcase "TEST123" 4 7)
  =>
  "TEST123"
) ;check
(check (string-downcase "aBc123XyZ" 1 7)
  =>
  "abc123xyZ"
) ;check

;; ; Error handling tests
(check-catch 'out-of-range
  (string-downcase "ABC" -1)
) ;check-catch
(check-catch 'out-of-range
  (string-downcase "ABC" 4)
) ;check-catch
(check (string-downcase "ABC" 0 0)
  =>
  "ABC"
) ;check
(check-catch 'out-of-range
  (string-downcase "ABC" 0 4)
) ;check-catch
(check-catch 'out-of-range
  (string-downcase "ABC" 2 1)
) ;check-catch
(check-catch 'out-of-range
  (string-downcase "" 1)
) ;check-catch
(check-catch 'out-of-range
  (string-downcase "A" 2)
) ;check-catch
(check-catch 'out-of-range
  (string-downcase "ABC" 3 6)
) ;check-catch
(check-catch 'out-of-range
  (string-downcase "ABC" 5 7)
) ;check-catch

;; ; Invalid argument type tests
(check-catch 'wrong-type-arg
  (string-downcase 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-downcase "hello" "123")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-downcase "hello" 1.5)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-downcase "hello" 1 4.5)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-downcase 'a)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-downcase "hello" 'a 'b)
) ;check-catch

;; ; Long strings and performance considerations
(check (string-downcase (make-string 100 #\A))
  =>
  (make-string 100 #\a)
) ;check
(check (string-downcase (make-string 1000 #\A))
  =>
  (make-string 1000 #\a)
) ;check

;; ; Special case consistency
(check (string-downcase "Test" 0 1)
  =>
  "test"
) ;check
(check (string-downcase "Test" 1 2)
  =>
  "Test"
) ;check
(check (string-downcase "Test" 2 3)
  =>
  "Test"
) ;check
(check (string-downcase "Test" 3 4)
  =>
  "Test"
) ;check
(check (string-downcase "Test " 0)
  =>
  "test "
) ;check
(check (string-downcase " Test")
  =>
  " test"
) ;check

(check (string-downcase "ABC" 0 1)
  =>
  "aBC"
) ;check

(check-report)
