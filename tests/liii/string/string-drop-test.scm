(import (liii check) (liii string))

;; string-drop
;; 从字符串开头移除指定数量的字符。
;;
;; 语法
;; ----
;; (string-drop str k)
;;
;; 参数
;; ----
;; str : string?
;; 源字符串，从中移除字符。
;;
;; k : integer?
;; 要移除的字符数量，必须是非负整数且不超过字符串长度。
;;
;; 返回值
;; ----
;; string
;; 返回一个新的字符串，包含源字符串从位置k开始的所有字符。
;;
;; 注意
;; ----
;; string-drop等价于(substring str k (string-length str))，但提供了更语义化的名称。
;; 对于多字节Unicode字符，操作基于字节位置而非字符位置。例如，每个中文字符占用3个字节，emoji字符通常占用4个字节。
;;
;; 示例
;; ----
;; (string-drop "MathAgape" 4) => "Agape"
;; (string-drop "Hello" 0) => "Hello"
;; (string-drop "abc" 2) => "c"
;; (string-drop "test123" 4) => "123"
;;
;; 错误处理
;; ----
;; out-of-range 当k大于字符串长度或k为负数时
;; wrong-type-arg 当str不是字符串类型或k不是整数类型时

(check (string-drop "MathAgape" 4)
  =>
  "Agape"
) ;check
(check (string-drop "MathAgape" 0)
  =>
  "MathAgape"
) ;check
(check (string-drop "MathAgape" 9)
  =>
  ""
) ;check
(check (string-drop "MathAgape" 8)
  =>
  "e"
) ;check
(check (string-drop "MathAgape" 1)
  =>
  "athAgape"
) ;check
(check (string-drop "MathAgape" 2)
  =>
  "thAgape"
) ;check
(check (string-drop "MathAgape" 3)
  =>
  "hAgape"
) ;check
(check (string-drop "MathAgape" 5)
  =>
  "gape"
) ;check
(check (string-drop "MathAgape" 6)
  =>
  "ape"
) ;check
(check (string-drop "MathAgape" 7)
  =>
  "pe"
) ;check
(check (string-drop "" 0) => "")
(check (string-drop "a" 1) => "")
(check (string-drop "Hello" 1)
  =>
  "ello"
) ;check
(check (string-drop "Hello" 5) => "")
(check (string-drop "Hello" 0)
  =>
  "Hello"
) ;check
(check (string-drop "abc" 2) => "c")
(check (string-drop "abc" 1) => "bc")
(check (string-drop "test123" 4)
  =>
  "123"
) ;check
(check (string-drop "test123" 3)
  =>
  "t123"
) ;check
(check (string-drop "test123" 6) => "3")
(check (string-drop "test123" 7) => "")
(check (string-drop "中文测试" 6)
  =>
  "测试"
) ;check
(check (string-drop "中文测试" 3)
  =>
  "文测试"
) ;check
(check (string-drop "中文测试" 12)
  =>
  ""
) ;check
(check (string-drop "🌟🎉" 4)
  =>
  "🎉"
) ;check
(check (string-drop "🌟🎉" 8) => "")

(check-catch 'out-of-range
  (string-drop "MathAgape" 20)
) ;check-catch
(check-catch 'out-of-range
  (string-drop "" 1)
) ;check-catch
(check-catch 'out-of-range
  (string-drop "Hello" -1)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-drop 123 4)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-drop "MathAgape" "4")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-drop "MathAgape" 4.5)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-drop "MathAgape" 'a)
) ;check-catch

(check (string-drop "MathAgape" 8)
  =>
  "e"
) ;check
(check (string-drop "MathAgape" 9)
  =>
  ""
) ;check
(check (string-drop "MathAgape" 0)
  =>
  "MathAgape"
) ;check

(check-catch 'out-of-range
  (string-drop "MahtAgape" -1)
) ;check-catch
(check-catch 'out-of-range
  (string-drop "MathAgape" 20)
) ;check-catch

;; string-drop-right
;; 从字符串末尾移除指定数量的字符。
;;
;; 语法
;; ----
;; (string-drop-right str k)
;;
;; 参数
;; ----
;; str : string?
;; 源字符串，从中移除字符。
;;
;; k : integer?
;; 要移除的字符数量，必须是非负整数且不超过字符串长度。
;;
;; 返回值
;; ----
;; string
;; 返回一个新的字符串，包含源字符串从开始位置到(len-k)的所有字符，其中len为字符串长度。
;;
;; 注意
;; ----
;; string-drop-right等价于(substring str 0 (- len k))，但提供了更语义化的名称。
;; 对于多字节Unicode字符，操作基于字节位置而非字符位置。例如，每个中文字符占用3个字节，emoji字符通常占用4个字节。
;;
;; 示例
;; ----
;; (string-drop-right "MathAgape" 4) => "Math"
;; (string-drop-right "Hello" 0) => "Hello"
;; (string-drop-right "abc" 2) => "a"
;; (string-drop-right "test123" 3) => "test"
;;
;; 错误处理
;; ----
;; out-of-range 当k大于字符串长度或k为负数时
;; wrong-type-arg 当str不是字符串类型或k不是整数类型时

(check (string-drop-right "MathAgape" 4)
  =>
  "MathA"
) ;check
(check (string-drop-right "MathAgape" 0)
  =>
  "MathAgape"
) ;check
(check (string-drop-right "MathAgape" 9)
  =>
  ""
) ;check
(check (string-drop-right "MathAgape" 8)
  =>
  "M"
) ;check
(check (string-drop-right "MathAgape" 1)
  =>
  "MathAgap"
) ;check
(check (string-drop-right "MathAgape" 2)
  =>
  "MathAga"
) ;check
(check (string-drop-right "MathAgape" 3)
  =>
  "MathAg"
) ;check
(check (string-drop-right "MathAgape" 5)
  =>
  "Math"
) ;check
(check (string-drop-right "MathAgape" 6)
  =>
  "Mat"
) ;check
(check (string-drop-right "MathAgape" 7)
  =>
  "Ma"
) ;check
(check (string-drop-right "" 0) => "")
(check (string-drop-right "a" 1) => "")
(check (string-drop-right "Hello" 1)
  =>
  "Hell"
) ;check
(check (string-drop-right "Hello" 5)
  =>
  ""
) ;check
(check (string-drop-right "Hello" 0)
  =>
  "Hello"
) ;check
(check (string-drop-right "abc" 2)
  =>
  "a"
) ;check
(check (string-drop-right "abc" 1)
  =>
  "ab"
) ;check
(check (string-drop-right "test123" 3)
  =>
  "test"
) ;check
(check (string-drop-right "test123" 4)
  =>
  "tes"
) ;check
(check (string-drop-right "test123" 6)
  =>
  "t"
) ;check
(check (string-drop-right "test123" 7)
  =>
  ""
) ;check
(check (string-drop-right "中文测试" 6)
  =>
  "中文"
) ;check
(check (string-drop-right "中文测试" 3)
  =>
  "中文测"
) ;check
(check (string-drop-right "中文测试" 12)
  =>
  ""
) ;check
(check (string-drop-right "🌟🎉" 4)
  =>
  "🌟"
) ;check
(check (string-drop-right "🌟🎉" 8)
  =>
  ""
) ;check

(check-catch 'out-of-range
  (string-drop-right "MathAgape" 20)
) ;check-catch
(check-catch 'out-of-range
  (string-drop-right "" 1)
) ;check-catch
(check-catch 'out-of-range
  (string-drop-right "Hello" -1)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-drop-right 123 4)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-drop-right "MathAgape" "4")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-drop-right "MathAgape" 4.5)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-drop-right "MathAgape" 'a)
) ;check-catch

(check (string-drop-right "MathAgape" 5)
  =>
  "Math"
) ;check
(check (string-drop-right "MathAgape" 9)
  =>
  ""
) ;check
(check (string-drop-right "MathAgape" 0)
  =>
  "MathAgape"
) ;check

(check-catch 'out-of-range
  (string-drop-right "MathAgape" -1)
) ;check-catch
(check-catch 'out-of-range
  (string-drop-right "MathAgape" 20)
) ;check-catch

(check-report)
