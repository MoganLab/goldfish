(import (liii check) (liii string))

;; string-take-right
;; 从字符串末尾提取指定数量的字符。
;;
;; 语法
;; ----
;; (string-take-right str k)
;;
;; 参数
;; ----
;; str : string?
;; 源字符串，从中提取字符。
;;
;; k : integer?
;; 要提取的字符数量，必须是非负整数且不超过字符串长度。
;;
;; 返回值
;; ----
;; string
;; 包含源字符串最后k个字符的新字符串。
;;
;; 注意
;; ----
;; string-take-right等价于(substring str (- (string-length str) k) (string-length str))，但提供了更语义化的名称。
;; 对于多字节Unicode字符，操作基于字节位置而非字符位置。例如，每个中文字符占用3个字节，emoji字符通常占用4个字节。
;;
;; 示例
;; ----
;; (string-take-right "MathAgape" 4) => "gape"
;; (string-take-right "Hello" 0) => ""
;; (string-take-right "abc" 2) => "bc"
;;
;; 错误处理
;; ----
;; out-of-range 当k大于字符串长度或k为负数时
;; wrong-type-arg 当str不是字符串类型或k不是整数类型时

(check (string-take-right "MathAgape" 0)
  =>
  ""
) ;check
(check (string-take-right "MathAgape" 1)
  =>
  "e"
) ;check
(check (string-take-right "MathAgape" 9)
  =>
  "MathAgape"
) ;check
(check (string-take-right "MathAgape" 4)
  =>
  "gape"
) ;check
(check (string-take-right "MathAgape" 0)
  =>
  ""
) ;check
(check (string-take-right "MathAgape" 9)
  =>
  "MathAgape"
) ;check
(check (string-take-right "" 0) => "")
(check (string-take-right "a" 1) => "a")
(check (string-take-right "Hello" 1)
  =>
  "o"
) ;check
(check (string-take-right "abc" 2)
  =>
  "bc"
) ;check
(check (string-take-right "test123" 3)
  =>
  "123"
) ;check
(check (string-take-right "中文测试" 6)
  =>
  "测试"
) ;check
(check (string-take-right "🌟🎉" 4)
  =>
  "🎉"
) ;check

(check-catch 'out-of-range
  (string-take-right "MathAgape" 20)
) ;check-catch
(check-catch 'out-of-range
  (string-take-right "" 1)
) ;check-catch
(check-catch 'out-of-range
  (string-take-right "Hello" -1)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-take-right 123 4)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-take-right "MathAgape" "4")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-take-right "MathAgape" 4.5)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-take-right "MathAgape" 'a)
) ;check-catch

(check-report)
