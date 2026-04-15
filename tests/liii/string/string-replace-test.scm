(import (liii check) (liii string))

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
(check (string-replace "hello world hello"
         "hello"
         "hi"
       ) ;string-replace
  =>
  "hi world hi"
) ;check
(check (string-replace "banana" "na" "N")
  =>
  "baNN"
) ;check

;; 边界条件测试
(check (string-replace "" "hello" "hi")
  =>
  ""
) ;check
(check (string-replace "hello world"
         "test"
         "hi"
       ) ;string-replace
  =>
  "hello world"
) ;check
(check (string-replace "hello" "" "x")
  =>
  "xhxexlxlxox"
) ;check
(check (string-replace "" "" "x")
  =>
  "x"
) ;check
(check (string-replace "hello world hello"
         "hello"
         ""
       ) ;string-replace
  =>
  " world "
) ;check
(check (string-replace "hello" "l" "")
  =>
  "heo"
) ;check

;; 非重叠、从左到右扫描
(check (string-replace "aaaa" "aa" "b")
  =>
  "bb"
) ;check
(check (string-replace "aaa" "a" "aa")
  =>
  "aaaaaa"
) ;check

;; Unicode 支持
(check (string-replace "测试测试字符串"
         "测试"
         "实验"
       ) ;string-replace
  =>
  "实验实验字符串"
) ;check
(check (string-replace "你好，世界"
         "世界"
         "Goldfish"
       ) ;string-replace
  =>
  "你好，Goldfish"
) ;check
(check (string-replace "你好世界"
         "世界"
         ""
       ) ;string-replace
  =>
  "你好"
) ;check
(check (string-replace "hello😀world😀"
         "😀"
         "!"
       ) ;string-replace
  =>
  "hello!world!"
) ;check

;; 特殊字符测试
(check (string-replace "hello world" " " "_")
  =>
  "hello_world"
) ;check
(check (string-replace "a\nb\nc" "\n" "")
  =>
  "abc"
) ;check
(check (string-replace "a\tb" "\t" "    ")
  =>
  "a    b"
) ;check

;; 边界位置测试
(check (string-replace "hello" "he" "X")
  =>
  "Xllo"
) ;check
(check (string-replace "hello" "lo" "X")
  =>
  "helX"
) ;check
(check (string-replace "hello" "hello" "world")
  =>
  "world"
) ;check
(check (string-replace "hello" "l" "l")
  =>
  "hello"
) ;check

;; 连续匹配测试
(check (string-replace "ababab" "ab" "X")
  =>
  "XXX"
) ;check
(check (string-replace "aaa" "a" "")
  =>
  ""
) ;check

;; pattern 长度相关
(check (string-replace "hi" "hello" "world")
  =>
  "hi"
) ;check
(check (string-replace "hello" "hello" "world")
  =>
  "world"
) ;check

;; 大小写敏感测试
(check (string-replace "Hello" "h" "H")
  =>
  "Hello"
) ;check
(check (string-replace "Hello" "H" "h")
  =>
  "hello"
) ;check

;; 数字字符串测试
(check (string-replace "123123" "12" "X")
  =>
  "X3X3"
) ;check

;; 返回副本而不是原对象
(let ((original "hello world")
      (modified (string-replace "hello world"
                  "test"
                  "hi"
                ) ;string-replace
      ) ;modified
     ) ;
  (check-true (equal? modified "hello world")
  ) ;check-true
  (check-false (eq? original modified))
) ;let

;; count 参数测试
;; count = 1, 2, 0 的基本用法
(check (string-replace "hello hello hello"
         "hello"
         "hi"
         1
       ) ;string-replace
  =>
  "hi hello hello"
) ;check
(check (string-replace "hello hello hello"
         "hello"
         "hi"
         2
       ) ;string-replace
  =>
  "hi hi hello"
) ;check
(check (string-replace "hello world"
         "hello"
         "hi"
         0
       ) ;string-replace
  =>
  "hello world"
) ;check

;; 负数 count（替换所有）
(check (string-replace "hello hello"
         "hello"
         "hi"
         -1
       ) ;string-replace
  =>
  "hi hi"
) ;check
(check (string-replace "a a a" "a" "b" -100)
  =>
  "b b b"
) ;check

;; count 超过实际匹配数
(check (string-replace "hello hello"
         "hello"
         "hi"
         10
       ) ;string-replace
  =>
  "hi hi"
) ;check

;; 从左到右的替换顺序
(check (string-replace "ababab" "ab" "X" 2)
  =>
  "XXab"
) ;check
(check (string-replace "aaa" "a" "b" 1)
  =>
  "baa"
) ;check
(check (string-replace "aaa" "a" "b" 2)
  =>
  "bba"
) ;check

;; 空 pattern 时的 count 行为
(check (string-replace "hello" "" "-" 1)
  =>
  "-hello"
) ;check
(check (string-replace "hello" "" "-" 2)
  =>
  "-h-ello"
) ;check
(check (string-replace "ab" "" "-" 5)
  =>
  "-a-b-"
) ;check
(check (string-replace "hello" "" "-" 0)
  =>
  "hello"
) ;check
(check (string-replace "ab" "" "-" -1)
  =>
  "-a-b-"
) ;check

;; 空原串 count 行为
(check (string-replace "" "" "x" 1)
  =>
  "x"
) ;check
(check (string-replace "" "" "x" 0)
  =>
  ""
) ;check

;; 删除 count 行为
(check (string-replace "hello hello"
         "hello"
         ""
         1
       ) ;string-replace
  =>
  " hello"
) ;check
(check (string-replace "hello hello"
         "hello"
         ""
         2
       ) ;string-replace
  =>
  " "
) ;check

;; 错误处理测试
(check-catch 'type-error
  (string-replace 123 "a" "b")
) ;check-catch
(check-catch 'type-error
  (string-replace "abc" 123 "b")
) ;check-catch
(check-catch 'type-error
  (string-replace "abc" "a" 123)
) ;check-catch
(check-catch 'type-error
  (string-replace "abc" "a" "b" "c")
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-replace)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-replace "abc" "a")
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-replace "abc" "a" "b" 1 "extra")
) ;check-catch

(check-report)
