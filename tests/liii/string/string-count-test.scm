(import (liii check)
  (liii error)
  (liii string)
  (srfi srfi-13)
) ;import

;; string-count
;; 统计字符串中满足指定条件的字符数量。
;;
;; 语法
;; ----
;; (string-count str char/pred?)
;; (string-count str char/pred? start)
;; (string-count str char/pred? start end)
;;
;; 参数
;; ----
;; str : string?
;; 要搜索的源字符串。
;;
;; char/pred? : char? 或 procedure?
;; - 字符(char)：统计字符串中与该字符相等的字符数量
;; - 谓词(procedure)：接受单个字符作为参数，返回布尔值的函数，统计使谓词返回#t的字符数量
;;
;; start : integer? 可选
;; 搜索的起始位置(包含)，默认为0。
;;
;; end : integer? 可选
;; 搜索的结束位置(不包含)，默认为字符串长度。
;;
;; 返回值
;; ----
;; integer
;; 返回在指定范围内满足条件的字符数量。
;;
;; 注意
;; ----
;; string-count会从字符串中统计符合指定条件的字符总数，支持单一字符匹配和谓词函数匹配两种模式。
;; 该函数支持start和end参数来限定搜索范围。
;; 对于空字符串或空范围会返回0。
;;
;; 错误处理
;; ----
;; type-error 当str不是字符串类型时
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

;; 基本功能测试 - 字符参数
(check (string-count "hello" #\l) => 2)
(check (string-count "hello" #\e) => 1)
(check (string-count "hello" #\z) => 0)
(check (string-count "" #\a) => 0)
(check (string-count "a" #\a) => 1)
(check (string-count "aaa" #\a) => 3)
(check (string-count "aAa" #\a) => 2)
(check (string-count "xyz" #\x) => 1)

;; 谓词参数测试
(check (string-count "123abc" char-numeric?)
  =>
  3
) ;check
(check (string-count "123abc" char-alphabetic?)
  =>
  3
) ;check
(check (string-count "hello" char-lower-case?)
  =>
  5
) ;check
(check (string-count "HELLO" char-upper-case?)
  =>
  5
) ;check
(check (string-count "Hello1221World"
         char-upper-case?
       ) ;string-count
  =>
  2
) ;check
(check (string-count "Hello1221World"
         char-lower-case?
       ) ;string-count
  =>
  8
) ;check
(check (string-count "   " char-whitespace?)
  =>
  3
) ;check
(check (string-count "hello world"
         char-whitespace?
       ) ;string-count
  =>
  1
) ;check
(check (string-count "abc123!@#"
         char-alphabetic?
       ) ;string-count
  =>
  3
) ;check
(check (string-count "!@#$%" char-alphabetic?)
  =>
  0
) ;check

;; 边界条件测试
(check (string-count "" char-numeric?)
  =>
  0
) ;check
(check (string-count "" char-alphabetic?)
  =>
  0
) ;check
(check (string-count "" char-whitespace?)
  =>
  0
) ;check

;; 单个字符边界测试
(check (string-count "a" char-alphabetic?)
  =>
  1
) ;check
(check (string-count "1" char-numeric?)
  =>
  1
) ;check
(check (string-count " " char-whitespace?)
  =>
  1
) ;check

;; 复杂字符组合测试
(check (string-count "a1 b2 c3"
         char-alphabetic?
       ) ;string-count
  =>
  3
) ;check
(check (string-count "a1 b2 c3" char-numeric?)
  =>
  3
) ;check
(check (string-count "a1 b2 c3"
         char-whitespace?
       ) ;string-count
  =>
  2
) ;check
(check (string-count "method123_doSomething456"
         char-alphabetic?
       ) ;string-count
  =>
  17
) ;check
(check (string-count "method123_doSomething456"
         char-numeric?
       ) ;string-count
  =>
  6
) ;check

;; 特殊字符测试
(check (string-count "特殊abc"
         char-alphabetic?
       ) ;string-count
  =>
  3
) ;check

;; start/end 范围参数测试
(check (string-count "hello world" #\l 0)
  =>
  3
) ;check
(check (string-count "hello world" #\l 6)
  =>
  1
) ;check
(check (string-count "hello world" #\l 0 5)
  =>
  2
) ;check
(check (string-count "hello world" #\l 6 11)
  =>
  1
) ;check
(check (string-count "hello world" #\l 0 3)
  =>
  1
) ;check
(check (string-count "hello world" #\l 4 8)
  =>
  0
) ;check

;; 谓词与范围组合测试
(check (string-count "abc123ABC"
         char-lower-case?
         0
         6
       ) ;string-count
  =>
  3
) ;check
(check (string-count "abc123ABC"
         char-upper-case?
         3
         9
       ) ;string-count
  =>
  3
) ;check
(check (string-count "Programming123"
         char-numeric?
         11
       ) ;string-count
  =>
  3
) ;check
(check (string-count "123456789"
         char-numeric?
         3
         6
       ) ;string-count
  =>
  3
) ;check

;; 空范围测试
(check (string-count "hello" #\l 0 0)
  =>
  0
) ;check
(check (string-count "hello" #\l 3 3)
  =>
  0
) ;check
(check (string-count "hello" #\l 5 5)
  =>
  0
) ;check
(check (string-count "hello"
         char-lower-case?
         2
         2
       ) ;string-count
  =>
  0
) ;check

;; 全范围测试
(check (string-count "hello" #\e 0)
  =>
  1
) ;check
(check (string-count "hello" #\e 0 5)
  =>
  1
) ;check

;; 自定义谓词测试
(check (string-count "hello world"
         (lambda (c)
           (or (char=? c #\l) (char=? c #\o))
         ) ;lambda
       ) ;string-count
  =>
  5
) ;check
(check (string-count "test123"
         (lambda (c)
           (or (char=? c #\t)
             (char=? c #\s)
             (char=? c #\e)
           ) ;or
         ) ;lambda
       ) ;string-count
  =>
  4
) ;check
(check (string-count "SPECIAL#chars"
         (lambda (c) (not (char-alphabetic? c)))
       ) ;string-count
  =>
  1
) ;check

;; 原有测试案例确保向后兼容
(check (string-count "xyz" #\x) => 1)
(check (string-count "xyz" #\x 0 1)
  =>
  1
) ;check
(check (string-count "xyz" #\y 0 1)
  =>
  0
) ;check
(check (string-count "xyz" #\x 0 3)
  =>
  1
) ;check
(check (string-count "xyz"
         (lambda (x) (char=? x #\x))
       ) ;string-count
  =>
  1
) ;check

;; 错误处理测试
(check-catch 'type-error
  (string-count 123 #\a)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-count "hello" 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-count "hello" "a")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-count "hello" '(a b c))
) ;check-catch

;; 参数数量错误测试
(check-catch 'wrong-number-of-args
  (string-count)
) ;check-catch
(check-catch 'wrong-number-of-args
  (string-count "hello")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-count "hello" #\l "invalid")
) ;check-catch

;; 范围越界测试
(check-catch 'out-of-range
  (string-count "hello" #\l -1)
) ;check-catch
(check-catch 'out-of-range
  (string-count "hello" #\l 0 10)
) ;check-catch
(check-catch 'out-of-range
  (string-count "hello" #\l 5 1)
) ;check-catch
(check-catch 'out-of-range
  (string-count "" #\l 1 2)
) ;check-catch
(check-catch 'out-of-range
  (string-count "hello" #\l 3 7)
) ;check-catch

(check-report)
