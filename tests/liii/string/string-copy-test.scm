(import (liii check) (liii string))

;; string-copy
;; 创建字符串的副本，支持可选的开始和结束位置参数进行子串拷贝。
;;
;; 语法
;; ----
;; (string-copy str)
;; (string-copy str start)
;; (string-copy str start end)
;;
;; 参数
;; ----
;; str : string?
;; 要复制的源字符串。
;;
;; start : integer? 可选
;; 复制开始的位置索引（包含），默认为0。
;;
;; end : integer? 可选
;; 复制结束的位置索引（不包含），默认为字符串长度。
;;
;; 返回值
;; ----
;; string
;; 返回源字符串的深拷贝，与源字符串内容相同但为不同的对象。
;;
;; 注意
;; ----
;; string-copy创建的是字符串内容的完整副本，即使内容与源字符串相同，
;; 返回的也是新的字符串对象，这一点可以通过eq?函数验证。
;;
;; 与substring函数不同，string-copy始终返回新的字符串对象，
;; 而substring在某些实现中可能会返回源字符串本身（当子串与源字符串相同时）。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当str不是字符串类型时
;; out-of-range 当start或end超出字符串索引范围时
;; out-of-range 当start > end时

(check-true (equal? (string-copy "hello") "hello")
) ;check-true
(check-true (equal? (string-copy "hello" 1) "ello")
) ;check-true
(check-true (equal? (string-copy "hello" 1 4) "ell")
) ;check-true
(check-true (equal? (string-copy "") "")
) ;check-true
(check-true (equal? (string-copy "中文测试")
              "中文测试"
            ) ;equal?
) ;check-true
(check-true (equal? (string-copy "中文测试" 6)
              "测试"
            ) ;equal?
) ;check-true
(check-true (equal? (string-copy "中文测试" 0 6)
              "中文"
            ) ;equal?
) ;check-true

(check-true (equal? (string-copy "hello" 0) "hello")
) ;check-true
(check-true (equal? (string-copy "hello" 5) "")
) ;check-true
(check-true (equal? (string-copy "abc" 0 0) "")
) ;check-true
(check-true (equal? (string-copy "abc" 0 1) "a")
) ;check-true
(check-true (equal? (string-copy "abc" 0 2) "ab")
) ;check-true
(check-true (equal? (string-copy "abc" 0 3) "abc")
) ;check-true

(check-false (eq? (string-copy "hello") "hello")
) ;check-false

(let ((original "hello"))
  (check-true (string=? (string-copy original)
                original
              ) ;string=?
  ) ;check-true
  (check-false (eq? (string-copy original) original)
  ) ;check-false
) ;let

(check-true (equal? (string-copy "test123" 0 4)
              "test"
            ) ;equal?
) ;check-true
(check-true (equal? (string-copy "test123" 4 7)
              "123"
            ) ;equal?
) ;check-true

(check-true (equal? (string-copy "🌟🎉" 0 4)
              "🌟"
            ) ;equal?
) ;check-true
(check-true (equal? (string-copy "🌟🎉" 4 8)
              "🎉"
            ) ;equal?
) ;check-true

(check-catch 'wrong-type-arg
  (string-copy 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-copy 'hello)
) ;check-catch
(check-catch 'out-of-range
  (string-copy "hello" -1)
) ;check-catch
(check-catch 'out-of-range
  (string-copy "hello" 10)
) ;check-catch
(check-catch 'out-of-range
  (string-copy "hello" 0 10)
) ;check-catch
(check-catch 'out-of-range
  (string-copy "" 1)
) ;check-catch
(check-catch 'out-of-range
  (string-copy "hello" 3 2)
) ;check-catch
(check-catch 'out-of-range
  (string-copy "hello" 4 3)
) ;check-catch

(check-catch 'wrong-type-arg
  (string-copy "hello" "a")
) ;check-catch
(check-catch 'wrong-type-arg
  (string-copy "hello" 1.5)
) ;check-catch
(check-catch 'wrong-type-arg
  (string-copy "hello" 1 4.5)
) ;check-catch

(check-report)
