(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; string-ci=?
;; 按大小写不敏感的方式比较多个字符串是否相等。
;;
;; 语法
;; ----
;; (string-ci=? string1 string2 ...)
;;
;; 参数
;; ----
;; string1, string2, ... : string?
;; 至少两个字符串参数。
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有字符串在大小写不敏感情况下相等则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 至少需要两个参数
;; 2. 所有参数必须是字符串类型
;; 3. 字符串比较不区分大小写（大小写等价）
;; 4. 当所有字符串内容在不区分大小写情况下相同返回 #t
;; 5. 返回布尔值结果
;;
;; 边界情况
;; --------
;; - 大小写不敏感：大写和小写字母被视为相同
;; - 空字符串比较：空字符串只与空字符串相等
;; - 特殊字符：所有字符需要内容相同，大小写不影响字母字符
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 当参数数量少于2个时抛出错误。
;; string-ci=? 基本测试
(check-true (string-ci=? "hello" "hello")
) ;check-true
(check-true (string-ci=? "hello" "HELLO")
) ;check-true
(check-true (string-ci=? "Hello" "hello")
) ;check-true
(check-true (string-ci=? "" ""))
(check-true (string-ci=? "a" "A"))
;; string-ci=? 大小写不敏感测试
(check-true (string-ci=? "HELLO" "hello")
) ;check-true
(check-true (string-ci=? "Hello" "HELLO")
) ;check-true
(check-true (string-ci=? "aBc" "AbC"))
(check-true (string-ci=? "UPPER" "upper")
) ;check-true
(check-true (string-ci=? "Mixed" "mixed")
) ;check-true
;; string-ci=? 不同内容测试
(check-false (string-ci=? "hello" "HELLOWORLD")
) ;check-false
(check-false (string-ci=? "abc" "def"))
(check-false (string-ci=? "short" "longer")
) ;check-false
(check-false (string-ci=? "test" "TESTING")
) ;check-false
;; string-ci=? 各种边界情况
(check-true (string-ci=? "123" "123"))
(check-true (string-ci=? "!@#$%" "!@#$%")
) ;check-true
(check-true (string-ci=? "空格 测试"
              "空格 测试"
            ) ;string-ci=?
) ;check-true
(check-false (string-ci=? "abc" "abcd"))
(check-false (string-ci=? "abcd" "abc"))
;; string-ci=? 特殊字符测试
(check-true (string-ci=? "\n\t" "\n\t"))
(check-true (string-ci=? "测试文本"
              "测试文本"
            ) ;string-ci=?
) ;check-true
(check-false (string-ci=? "测试" "测试文本")
) ;check-false
(check-false (string-ci=? "测试文本" "测试")
) ;check-false
;; string-ci=? 空字符串测试
(check-true (string-ci=? "" "" ""))
(check-true (string-ci=? "a" "A" "a"))
;; string-ci=? 多参数测试
(check-true (string-ci=? "same" "SAME" "Same")
) ;check-true
(check-false (string-ci=? "same" "DIFF" "same")
) ;check-false
(check-false (string-ci=? "ONE" "two" "Three")
) ;check-false
;; string-ci=? 二进制和Unicode字符串
(check-true (string-ci=? "Hello, 世界!"
              "hello, 世界!"
            ) ;string-ci=?
) ;check-true
(check-true (string-ci=? "JAVA" "java" "Java")
) ;check-true
(check-false (string-ci=? "Hello" "HELLOWORLD")
) ;check-false
;; string-ci=? 大小写混合场景
(check-true (string-ci=? "Goldfish Scheme"
              "goldfish scheme"
            ) ;string-ci=?
) ;check-true
(check-true (string-ci=? "r7rs" "R7RS" "R7rs")
) ;check-true
(check-true (string-ci=? "CLaUdE" "claude")
) ;check-true
(check-false (string-ci=? "TeXMACS" "textmacs")
) ;check-false
;; 错误处理测试
(check-catch 'wrong-number-of-args
  (string-ci=?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-ci=? "hello")
) ;check-catch
(check-report)
