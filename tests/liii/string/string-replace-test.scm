(import (liii check)
        (liii string))

;; string-replace
;; 按从左到右、非重叠的方式替换字符串中的所有匹配子串。
;;
;; 语法
;; ----
;; (string-replace str old new [count])
;;
;; 参数
;; ----
;; str : string?
;; 要处理的源字符串。
;;
;; old : string?
;; 要被查找并替换的子字符串。
;;
;; new : string?
;; 用于替换的新字符串。
;;
;; count : integer? (可选)
;; 最大替换次数。count > 0 时最多替换 count 次；count = 0 时不替换；
;; count < 0 或不传时替换所有匹配。
;;
;; 返回值
;; ----
;; string
;; 返回一个新的字符串，其中str里所有 old 的非重叠匹配都被替换为 new。
;;
;; 注意
;; ----
;; - 这是一个更符合日常编码直觉的 replace：默认替换全部匹配。
;; - 替换过程按原字符串从左到右扫描，不会重复扫描刚刚插入的 new。
;; - 当 old 为空字符串时，在每个字符之间插入 new（Python 兼容行为）。
;; - 如果没有匹配，返回原内容的副本。
;;
;; 错误处理
;; ----
;; type-error 当任一参数不是字符串类型时
;; wrong-number-of-args 当参数数量不正确时

;; 基本功能测试
(check (string-replace "hello world hello" "hello" "hi") => "hi world hi")
(check (string-replace "banana" "na" "N") => "baNN")

;; 边界条件测试
(check (string-replace "" "hello" "hi") => "")
(check (string-replace "hello world" "test" "hi") => "hello world")
(check (string-replace "hello" "" "x") => "xhxexlxlxox")  ;; Python 兼容: 空pattern在字符间插入
(check (string-replace "" "" "x") => "x")  ;; Python 兼容: 空串+空pattern=new
(check (string-replace "hello world hello" "hello" "") => " world ")
(check (string-replace "hello" "l" "") => "heo")  ;; 删除匹配字符

;; 非重叠、从左到右扫描
(check (string-replace "aaaa" "aa" "b") => "bb")
(check (string-replace "aaa" "a" "aa") => "aaaaaa")

;; Unicode 支持
(check (string-replace "测试测试字符串" "测试" "实验") => "实验实验字符串")
(check (string-replace "你好，世界" "世界" "Goldfish") => "你好，Goldfish")
(check (string-replace "你好世界" "世界" "") => "你好")  ;; Unicode 删除
(check (string-replace "hello😀world😀" "😀" "!") => "hello!world!")  ;; Emoji

;; 特殊字符测试
(check (string-replace "hello world" " " "_") => "hello_world")
(check (string-replace "a\nb\nc" "\n" "") => "abc")  ;; 换行符
(check (string-replace "a\tb" "\t" "    ") => "a    b")  ;; 制表符

;; 边界位置测试
(check (string-replace "hello" "he" "X") => "Xllo")  ;; 开头匹配
(check (string-replace "hello" "lo" "X") => "helX")  ;; 结尾匹配
(check (string-replace "hello" "hello" "world") => "world")  ;; 整串匹配
(check (string-replace "hello" "l" "l") => "hello")  ;; 替换为相同内容

;; 连续匹配测试
(check (string-replace "ababab" "ab" "X") => "XXX")
(check (string-replace "aaa" "a" "") => "")  ;; 全部删除

;; pattern 长度相关
(check (string-replace "hi" "hello" "world") => "hi")  ;; pattern 比原串长
(check (string-replace "hello" "hello" "world") => "world")  ;; pattern 等于原串

;; 大小写敏感测试
(check (string-replace "Hello" "h" "H") => "Hello")  ;; H != h
(check (string-replace "Hello" "H" "h") => "hello")

;; 数字字符串测试
(check (string-replace "123123" "12" "X") => "X3X3")

;; 返回副本而不是原对象
(let ((original "hello world")
      (modified (string-replace "hello world" "test" "hi")))
  (check-true (equal? modified "hello world"))
  (check-false (eq? original modified))
) ;;let

;; count 参数测试
;; count = 1, 2, 0 的基本用法
(check (string-replace "hello hello hello" "hello" "hi" 1) => "hi hello hello")
(check (string-replace "hello hello hello" "hello" "hi" 2) => "hi hi hello")
(check (string-replace "hello world" "hello" "hi" 0) => "hello world")

;; 负数 count（替换所有）
(check (string-replace "hello hello" "hello" "hi" -1) => "hi hi")
(check (string-replace "a a a" "a" "b" -100) => "b b b")

;; count 超过实际匹配数
(check (string-replace "hello hello" "hello" "hi" 10) => "hi hi")

;; 从左到右的替换顺序
(check (string-replace "ababab" "ab" "X" 2) => "XXab")
(check (string-replace "aaa" "a" "b" 1) => "baa")
(check (string-replace "aaa" "a" "b" 2) => "bba")

;; 空 pattern 时的 count 行为
(check (string-replace "hello" "" "-" 1) => "-hello")
(check (string-replace "hello" "" "-" 2) => "-h-ello")
(check (string-replace "ab" "" "-" 5) => "-a-b-")
(check (string-replace "hello" "" "-" 0) => "hello")
(check (string-replace "ab" "" "-" -1) => "-a-b-")

;; 空原串 count 行为
(check (string-replace "" "" "x" 1) => "x")
(check (string-replace "" "" "x" 0) => "")

;; 删除 count 行为
(check (string-replace "hello hello" "hello" "" 1) => " hello")
(check (string-replace "hello hello" "hello" "" 2) => " ")

;; 错误处理测试
(check-catch 'type-error (string-replace 123 "a" "b"))
(check-catch 'type-error (string-replace "abc" 123 "b"))
(check-catch 'type-error (string-replace "abc" "a" 123))
(check-catch 'type-error (string-replace "abc" "a" "b" "c"))  ;; count 必须是整数
(check-catch 'wrong-number-of-args (string-replace))
(check-catch 'wrong-number-of-args (string-replace "abc" "a"))
(check-catch 'wrong-number-of-args (string-replace "abc" "a" "b" 1 "extra"))  ;; 参数过多

(check-report)
