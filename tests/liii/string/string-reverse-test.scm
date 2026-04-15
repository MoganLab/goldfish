(import (liii check) (liii string))

;; string-reverse
;; 反转字符串的字符顺序。
;;
;; 语法
;; ----
;; (string-reverse str)
;; (string-reverse str start)
;; (string-reverse str start end)
;;
;; 参数
;; ----
;; str : string?
;; 要反转的源字符串。
;;
;; start : integer? 可选
;; 反转操作的起始位置索引（包含），默认为0。
;;
;; end : integer? 可选
;; 反转操作的结束位置索引（不包含），默认为字符串长度。
;;
;; 返回值
;; ----
;; string
;; 返回一个新的字符串，其字符顺序与源字符串相反。
;;
;; 注意
;; ----
;; string-reverse会将字符串中的字符顺序完全颠倒过来。
;; 当指定start和end参数时，仅反转指定范围内的字符，范围外的字符保持不变。
;;
;; 错误处理
;; ----
;; out-of-range 当start/end超出字符串索引范围时
;; wrong-type-arg 当str不是字符串类型时

(check (string-reverse "01234")
  =>
  "43210"
) ;check
(check (string-reverse "hello")
  =>
  "olleh"
) ;check
(check (string-reverse "hello world")
  =>
  "dlrow olleh"
) ;check
(check (string-reverse "abc123")
  =>
  "321cba"
) ;check
(check (string-reverse "") => "")
(check (string-reverse "a") => "a")
(check (string-reverse "ab") => "ba")
(check (string-reverse "abc") => "cba")
(check (string-reverse "A1B2C3")
  =>
  "3C2B1A"
) ;check

(check (string-reverse "x") => "x")
(check (string-reverse "1") => "1")
(check (string-reverse "z") => "z")

(check (string-reverse "") => "")
(check (string-reverse "" 0) => "")
(check (string-reverse "" 0 0) => "")

(check (string-reverse "racecar")
  =>
  "racecar"
) ;check
(check (string-reverse "A man, a plan, a canal, Panama"
       ) ;string-reverse
  =>
  "amanaP ,lanac a ,nalp a ,nam A"
) ;check
(check (string-reverse "aba") => "aba")
(check (string-reverse "abba")
  =>
  "abba"
) ;check

(check (string-reverse "1234567890")
  =>
  "0987654321"
) ;check
(check (string-reverse "12345")
  =>
  "54321"
) ;check
(check (string-reverse "1001")
  =>
  "1001"
) ;check

(check (string-reverse "01234" 0)
  =>
  "43210"
) ;check
(check (string-reverse "01234" 1)
  =>
  "04321"
) ;check
(check (string-reverse "01234" 2)
  =>
  "01432"
) ;check
(check (string-reverse "01234" 3)
  =>
  "01243"
) ;check
(check (string-reverse "01234" 4)
  =>
  "01234"
) ;check
(check (string-reverse "01234" 5)
  =>
  "01234"
) ;check

(check (string-reverse "01234" 0 2)
  =>
  "10234"
) ;check
(check (string-reverse "01234" 0 3)
  =>
  "21034"
) ;check
(check (string-reverse "01234" 1 3)
  =>
  "02134"
) ;check
(check (string-reverse "01234" 1 4)
  =>
  "03214"
) ;check
(check (string-reverse "01234" 2 4)
  =>
  "01324"
) ;check
(check (string-reverse "01234" 0 5)
  =>
  "43210"
) ;check
(check (string-reverse "hello" 1 4)
  =>
  "hlleo"
) ;check
(check (string-reverse "abcdef" 1 4)
  =>
  "adcbef"
) ;check

(check (string-reverse "test string" 0 0)
  =>
  "test string"
) ;check
(check (string-reverse "test string" 3 3)
  =>
  "test string"
) ;check
(check (string-reverse "test string" 11 11)
  =>
  "test string"
) ;check
(check (string-reverse "abcdefghij" 5)
  =>
  "abcdejihgf"
) ;check
(check (string-reverse "reverse" 2)
  =>
  "reesrev"
) ;check

(check (string-reverse "hello" 0 1)
  =>
  "hello"
) ;check
(check (string-reverse "hello" 4 5)
  =>
  "hello"
) ;check
(check (string-reverse "hello" 1 2)
  =>
  "hello"
) ;check
(check (string-reverse "longertext" 8 9)
  =>
  "longertext"
) ;check

(check (string-reverse "abcd" 0 2)
  =>
  "bacd"
) ;check
(check (string-reverse "abcd" 1 3)
  =>
  "acbd"
) ;check
(check (string-reverse "abcd" 2 4)
  =>
  "abdc"
) ;check

(check (string-reverse "abcdef"
         0
         (string-length "abcdef")
       ) ;string-reverse
  =>
  "fedcba"
) ;check
(check (string-reverse "programming" 0 11)
  =>
  "gnimmargorp"
) ;check


(check (string-reverse "a") => "a")
(check (string-reverse "abc") => "cba")

(check (string? (string-reverse "中"))
  =>
  #t
) ;check
(check (= (string-length (string-reverse "中"))
         (string-length "中")
       ) ;=
  =>
  #t
) ;check

(check (string? (string-reverse "中文"))
  =>
  #t
) ;check
(check (= (string-length (string-reverse "中文")
          ) ;string-length
         (string-length "中文")
       ) ;=
  =>
  #t
) ;check

(check (string? (string-reverse "国"))
  =>
  #t
) ;check
(check (= (string-length (string-reverse "国"))
         (string-length "国")
       ) ;=
  =>
  #t
) ;check

(check (string? (string-reverse "￥"))
  =>
  #t
) ;check
(check (= (string-length (string-reverse "￥"))
         (string-length "￥")
       ) ;=
  =>
  #t
) ;check

(check (string? (string-reverse "🙂"))
  =>
  #t
) ;check
(check (= (string-length (string-reverse "🙂"))
         (string-length "🙂")
       ) ;=
  =>
  #t
) ;check

(check (string? (string-reverse "👍"))
  =>
  #t
) ;check
(check (= (string-length (string-reverse "👍"))
         (string-length "👍")
       ) ;=
  =>
  #t
) ;check

(check (string? (string-reverse "🙂👍"))
  =>
  #t
) ;check
(check (= (string-length (string-reverse "🙂👍")
          ) ;string-length
         (string-length "🙂👍")
       ) ;=
  =>
  #t
) ;check

(check (string? (string-reverse "Hello世界"))
  =>
  #t
) ;check
(check (= (string-length (string-reverse "Hello世界")
          ) ;string-length
         (string-length "Hello世界")
       ) ;=
  =>
  #t
) ;check

(check (string? (string-reverse "测试🎉"))
  =>
  #t
) ;check
(check (= (string-length (string-reverse "测试🎉")
          ) ;string-length
         (string-length "测试🎉")
       ) ;=
  =>
  #t
) ;check

(check-catch 'out-of-range
  (string-reverse "01234" -1)
) ;check-catch
(check-catch 'out-of-range
  (string-reverse "01234" 6)
) ;check-catch
(check-catch 'out-of-range
  (string-reverse "01234" 5 4)
) ;check-catch
(check-catch 'out-of-range
  (string-reverse "01234" 1 6)
) ;check-catch
(check-catch 'out-of-range
  (string-reverse "01234" -1 3)
) ;check-catch
(check-catch 'out-of-range
  (string-reverse "01234" 3 1)
) ;check-catch
(check-catch 'out-of-range
  (string-reverse "" -1)
) ;check-catch
(check-catch 'out-of-range
  (string-reverse "test" 0 5)
) ;check-catch
(check-catch 'out-of-range
  (string-reverse "" 1)
) ;check-catch

(check-catch 'wrong-type-arg
  (string-reverse 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-reverse "hello" "not-number")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-reverse "hello" 1.5)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-reverse "hello" 1 2.5)
) ;check-catch

(check-report)
