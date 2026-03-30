(import (liii check)
        (liii string)
) ;import

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

;;; Basic functionality tests for string-downcase
(check (string-downcase "ABC") => "abc")
(check (string-downcase "abc") => "abc")
(check (string-downcase "ABC123") => "abc123")
(check (string-downcase "123ABC") => "123abc")
(check (string-downcase "Hello World") => "hello world")
(check (string-downcase "!@#$%") => "!@#$%")
(check (string-downcase "MixedCaseString") => "mixedcasestring")
(check (string-downcase "UPPERCASE") => "uppercase")
(check (string-downcase "lowercase") => "lowercase")
(check (string-downcase "CamelCaseISAGoodName") => "camelcaseisagoodname")
(check (string-downcase "") => "")
(check (string-downcase "A") => "a")
(check (string-downcase "Z") => "z")
(check (string-downcase "a1B2c3D4") => "a1b2c3d4")

;;; 验证非ASCII字符保持不变（当前实现只支持基本ASCII）
(check (string-downcase "中文") => "中文")
(check (string-downcase "中文TEST功能") => "中文test功能")
(check (string-downcase "ÀÁÂ") => "ÀÁÂ")
(check (string-downcase "À") => "À")
(check (string-downcase "Á") => "Á")
(check (string-downcase "ÄÖÜ") => "ÄÖÜ")
(check (string-downcase "ΑΒΓ") => "ΑΒΓ")
(check (string-downcase "café") => "café")

;;; Mixed alphanumeric and special characters
(check (string-downcase "ABC-def-GHI") => "abc-def-ghi")
(check (string-downcase "123-ABC-xyz") => "123-abc-xyz")
(check (string-downcase "___ABCDE___") => "___abcde___")
(check (string-downcase ".COM/.NET/.ORG") => ".com/.net/.org")

;;; Edge cases - single character
(check (string-downcase "X") => "x")
(check (string-downcase "x") => "x")
(check (string-downcase "0") => "0")
(check (string-downcase ".") => ".")
(check (string-downcase " ") => " ")

;;; String case variations
(check (string-downcase "ABCDEFGHIJKLMNOPQRSTUVWXYZ") => "abcdefghijklmnopqrstuvwxyz")
(check (string-downcase "abcdefghijklmnopqrstuvwxyz") => "abcdefghijklmnopqrstuvwxyz")
(check (string-downcase "AbCdEfGhIjKlMnOpQrStUvWxYz") => "abcdefghijklmnopqrstuvwxyz")
(check (string-downcase "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") => "0123456789abcdefghijklmnopqrstuvwxyz0123456789")

;;; Boundary conditions
(check (string-downcase "A" 0) => "a")
(check (string-downcase "ABC" 0) => "abc")
(check (string-downcase "ABC" 0 1) => "aBC")
(check (string-downcase "ABC" 0 2) => "abC")
(check (string-downcase "ABC" 0 3) => "abc")
(check (string-downcase "ABC" 1) => "Abc")
(check (string-downcase "ABC" 1 2) => "AbC")
(check (string-downcase "ABC" 1 3) => "Abc")
(check (string-downcase "ABC" 2) => "ABc")
(check (string-downcase "ABC" 2 3) => "ABc")

;;; ASCII boundary verification
(check (string-downcase "aBc" 0 1) => "aBc")
(check (string-downcase "aBc" 0 0) => "aBc") ; no change when start=end

;; Additional boundary tests
(check (string-downcase "A1B2C3D4E5" 0) => "a1b2c3d4e5")
(check (string-downcase "A1B2C3D4E5" 2) => "A1b2c3d4e5")
(check (string-downcase "A1B2C3D4E5" 2 5) => "A1b2c3D4E5")
(check (string-downcase "A1B2C3D4E5" 0 1) => "a1B2C3D4E5")

;;; Spanning over different character types
(check (string-downcase "TEST123" 0 4) => "test123")
(check (string-downcase "TEST123" 2 5) => "TEst123")
(check (string-downcase "TEST123" 4 7) => "TEST123")
(check (string-downcase "aBc123XyZ" 1 7) => "abc123xyZ")

;;; Error handling tests
(check-catch 'out-of-range (string-downcase "ABC" -1))
(check-catch 'out-of-range (string-downcase "ABC" 4))
(check (string-downcase "ABC" 0 0) => "ABC")
(check-catch 'out-of-range (string-downcase "ABC" 0 4))
(check-catch 'out-of-range (string-downcase "ABC" 2 1))  ; start > end
(check-catch 'out-of-range (string-downcase "" 1))
(check-catch 'out-of-range (string-downcase "A" 2))
(check-catch 'out-of-range (string-downcase "ABC" 3 6))
(check-catch 'out-of-range (string-downcase "ABC" 5 7))

;;; Invalid argument type tests
(check-catch 'wrong-type-arg (string-downcase 123))
(check-catch 'wrong-type-arg (string-downcase "hello" "123"))
(check-catch 'wrong-type-arg (string-downcase "hello" 1.5))
(check-catch 'wrong-type-arg (string-downcase "hello" 1 4.5))
(check-catch 'wrong-type-arg (string-downcase 'a))
(check-catch 'wrong-type-arg (string-downcase "hello" 'a 'b))

;;; Long strings and performance considerations
(check (string-downcase (make-string 100 #\A)) => (make-string 100 #\a))
(check (string-downcase (make-string 1000 #\A)) => (make-string 1000 #\a))

;;; Special case consistency
(check (string-downcase "Test" 0 1) => "test")
(check (string-downcase "Test" 1 2) => "Test")
(check (string-downcase "Test" 2 3) => "Test")
(check (string-downcase "Test" 3 4) => "Test")
(check (string-downcase "Test " 0) => "test ")
(check (string-downcase " Test") => " test")

(check (string-downcase "ABC" 0 1) => "aBC")

(check-report)
