(import (liii check) (liii string))

;; string-skip-right
;; 在字符串中从右向左跳过指定字符或满足条件的字符，返回第一个不满足条件的字符位置。
;;
;; 语法
;; ----
;; (string-skip-right str char/pred?)
;; (string-skip-right str char/pred? start)
;; (string-skip-right str char/pred? start end)
;;
;; 参数
;; ----
;; str : string?
;; 要搜索的源字符串。
;;
;; char/pred? : char? 或 procedure?
;; - 字符(char)：要跳过的目标字符
;; - 谓词(procedure)：接受单个字符作为参数的函数，返回布尔值指示是否跳过该字符
;;
;; start : integer? 可选
;; 搜索的起始位置(包含)，默认为0。
;;
;; end : integer? 可选
;; 搜索的结束位置(不包含)，默认为字符串长度。
;;
;; 返回值
;; ----
;; integer 或 #f
;; - 如果找到不匹配的字符，返回其索引位置(从0开始计数)
;; - 如果所有字符都匹配（都满足跳过条件），返回#f
;;
;; 注意
;; ----
;; string-skip-right从字符串的右侧(末尾)开始搜索，跳过所有满足条件的字符，返回第一个不满足条件的字符索引。
;; 搜索范围由start和end参数限定。如果指定范围内的所有字符都满足跳过条件，则返回#f。
;;
;; 该函数支持使用字符和谓词两种方式:
;; - 字符匹配：跳过与指定字符相等的字符
;; - 谓词匹配：跳过使谓词返回#t的字符
;;
;; string-skip-right是string-index-right的补充：string-index-right查找满足条件的字符，string-skip-right查找不满足条件的字符。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; wrong-type-arg 当char/pred?不是字符或谓词时
;; out-of-range 当start/end超出字符串索引范围时

;; 基本功能测试
(check (string-skip-right "hello   " #\space)
  =>
  4
) ;check
(check (string-skip-right "aaaa" #\a)
  =>
  #f
) ;check
(check (string-skip-right "abc123"
         char-numeric?
       ) ;string-skip-right
  =>
  2
) ;check
(check (string-skip-right "123abc"
         char-alphabetic?
       ) ;string-skip-right
  =>
  2
) ;check
(check (string-skip-right "" #\space)
  =>
  #f
) ;check

;; 字符参数测试
(check (string-skip-right "abcxxx" #\x)
  =>
  2
) ;check
(check (string-skip-right "abcxxx" #\x 0 5)
  =>
  2
) ;check
(check (string-skip-right "abcxxx" #\x 0 4)
  =>
  2
) ;check
(check (string-skip-right "   \t\n  "
         char-whitespace?
       ) ;string-skip-right
  =>
  #f
) ;check

;; 扩展综合测试
(check (string-skip-right "helloh" #\h)
  =>
  4
) ;check
(check (string-skip-right "hellohh" #\h)
  =>
  4
) ;check
(check (string-skip-right "hhh" #\h)
  =>
  #f
) ;check
(check (string-skip-right "hello world" #\d)
  =>
  9
) ;check
(check (string-skip-right "hello" #\x)
  =>
  4
) ;check
(check (string-skip-right "" #\a) => #f)
(check (string-skip-right "a" #\a)
  =>
  #f
) ;check
(check (string-skip-right "a" #\b) => 0)
(check (string-skip-right "0123456789" #\9)
  =>
  8
) ;check
(check (string-skip-right "0123456789" #\8)
  =>
  9
) ;check

;; 谓词参数测试
(check (string-skip-right "0123456789"
         char-numeric?
       ) ;string-skip-right
  =>
  #f
) ;check
(check (string-skip-right "abc123"
         char-numeric?
       ) ;string-skip-right
  =>
  2
) ;check
(check (string-skip-right "123abc"
         char-alphabetic?
       ) ;string-skip-right
  =>
  2
) ;check
(check (string-skip-right "Hello123"
         char-upper-case?
       ) ;string-skip-right
  =>
  7
) ;check
(check (string-skip-right "HELLO"
         char-upper-case?
       ) ;string-skip-right
  =>
  #f
) ;check
(check (string-skip-right "123!@#"
         char-alphabetic?
       ) ;string-skip-right
  =>
  5
) ;check
(check (string-skip-right "hello   "
         char-whitespace?
       ) ;string-skip-right
  =>
  4
) ;check
(check (string-skip-right "helloh"
         (lambda (c) (char=? c #\h))
       ) ;string-skip-right
  =>
  4
) ;check

;; 单字符边界情况
(check (string-skip-right "a" #\a)
  =>
  #f
) ;check
(check (string-skip-right "a" #\b) => 0)
(check (string-skip-right " " #\space)
  =>
  #f
) ;check
(check (string-skip-right "\t"
         char-whitespace?
       ) ;string-skip-right
  =>
  #f
) ;check

;; start和end参数测试
(check (string-skip-right "abcxxx" #\x 0)
  =>
  2
) ;check
(check (string-skip-right "abcxxx" #\x 1)
  =>
  2
) ;check
(check (string-skip-right "abcxxx" #\x 2)
  =>
  2
) ;check
(check (string-skip-right "abcxxx" #\x 3)
  =>
  #f
) ;check
(check (string-skip-right "abcxxx" #\x 4)
  =>
  #f
) ;check
(check (string-skip-right "abcxxx" #\x 0 3)
  =>
  2
) ;check
(check (string-skip-right "abcxxx" #\x 0 4)
  =>
  2
) ;check
(check (string-skip-right "abcxxx" #\x 1 4)
  =>
  2
) ;check
(check (string-skip-right "abcxxx" #\x 2 4)
  =>
  2
) ;check
(check (string-skip-right "abcxxx" #\x 3 4)
  =>
  #f
) ;check
(check (string-skip-right "abcxxx" #\x 3 3)
  =>
  #f
) ;check

;; 特殊字符和边界情况
(check (string-skip-right "test___" #\_)
  =>
  3
) ;check
(check (string-skip-right "b@@a" #\@)
  =>
  3
) ;check
(check (string-skip-right "a---" #\-)
  =>
  0
) ;check
(check (string-skip-right "hello,," #\,)
  =>
  4
) ;check

;; 复杂谓词
(check (string-skip-right "!@#abc123"
         (lambda (c)
           (or (char-alphabetic? c)
             (char-numeric? c)
           ) ;or
         ) ;lambda
       ) ;string-skip-right
  =>
  2
) ;check
(check (string-skip-right "abc123   "
         char-whitespace?
       ) ;string-skip-right
  =>
  5
) ;check
(check (string-skip-right "abc123"
         char-upper-case?
       ) ;string-skip-right
  =>
  5
) ;check
(check (string-skip-right "ABC123"
         char-upper-case?
       ) ;string-skip-right
  =>
  5
) ;check
(check (string-skip-right "abcABC"
         char-upper-case?
       ) ;string-skip-right
  =>
  2
) ;check

;; 错误处理测试
(check-catch 'wrong-type-arg
  (string-skip-right 123 #\a)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-skip-right "hello" "a")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-skip-right "hello" 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-skip-right "hello" '(a))
) ;check-catch
(check-catch 'out-of-range
  (string-skip-right "hello" #\a -1)
) ;check-catch
(check-catch 'out-of-range
  (string-skip-right "hello" #\a 0 6)
) ;check-catch
(check-catch 'out-of-range
  (string-skip-right "hello" #\a 3 2)
) ;check-catch
(check-catch 'out-of-range
  (string-skip-right "" #\a 1)
) ;check-catch
(check-catch 'out-of-range
  (string-skip-right "abc" #\a 5)
) ;check-catch

(check-report)
