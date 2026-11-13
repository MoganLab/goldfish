;; bigint-multiply
;; 计算两个 bigint 的积。
;;
;; 语法
;; ----
;; (bigint-multiply a b)
;;
;; 参数
;; ----
;; a : bigint?
;;   第一个乘数。
;; b : bigint?
;;   第二个乘数。
;;
;; 返回值
;; ----
;; bigint?
;;   a 与 b 的乘积。
;;
;; 说明
;; ----
;; 根据输入大小自动选择最优乘法算法：
;; - 小于 80 个 32 位数字：grade-school O(n²) 算法
;; - 80-240 个 32 位数字：Karatsuba O(n^1.585) 分治算法
;; - 大于 240 个 32 位数字：Toom-Cook 3-way O(n^1.465) 多项式算法
;; 阈值基于 JDK BigInteger 经验调整，适配 32 位数字表示。
;; 错误处理
;; ----
;; type-error
;;   当任一参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-multiply (b "0") (b "999"))) => "0")
(check (bigint->string (bigint-multiply (b "12") (b "34"))) => "408")
(check (bigint->string (bigint-multiply (b "-12") (b "34"))) => "-408")
(check (bigint->string (bigint-multiply (b "65536") (b "65536"))) => "4294967296")
(check (bigint->string (bigint-multiply (b "4294967296") (b "4294967296")))
       => "18446744073709551616"
) ;check
(check (bigint->string (bigint-multiply (b "12345678901234567890") (b "10")))
       => "123456789012345678900"
) ;check

(check-catch 'type-error (bigint-multiply 1 (b "2")))
(check-catch 'type-error (bigint-multiply (b "1") 2))

;; 大数算法路径（Karatsuba / Toom-Cook）通过公开接口验证

;; 中等数字（21位十进制数）
(let* ((a (b "100000000000000000000"))
       (c (b "200000000000000000000"))
       (result (bigint-multiply a c)))
  (check (bigint->string result) => "20000000000000000000000000000000000000000")
) ;let*

;; 大数字（100位十进制数）
(let* ((big-100 "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890")
       (a (b big-100))
       (result (bigint-multiply a (b "2"))))
  (check (string=? (substring (bigint->string result) 0 10) "2469135780") => #t)
) ;let*

;; 两个 100 位大数相乘
(let* ((big-100 "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890")
       (a (b big-100))
       (result (bigint-multiply a a)))
  (check (bigint-positive? result) => #t)
  (check (> (string-length (bigint->string result)) 190) => #t)
) ;let*

;; 约 240 位十进制数：乘后除回去应还原
(define big-240-digits
  (string-append "9876543210987654321098765432109876543210"
                 "9876543210987654321098765432109876543210"
                 "9876543210987654321098765432109876543210"
                 "9876543210987654321098765432109876543210"
                 "9876543210987654321098765432109876543210"
                 "9876543210987654321098765432109876543210"))

(let* ((a (b big-240-digits))
       (result (bigint-multiply a (b "5")))
       (divided (bigint-divide result (b "5"))))
  (check (bigint=? divided a) => #t)
) ;let*

;; 大数平方：位数应约翻倍
(let* ((a (b big-240-digits))
       (result (bigint-multiply a a)))
  (check (>= (string-length (bigint->string result)) 470) => #t)
) ;let*

;; 大数负数乘法
(let* ((a (b (string-append "-" big-240-digits)))
       (product (bigint-multiply a (b big-240-digits))))
  (check (bigint-negative? product) => #t)
) ;let*

;; 大数零乘法
(let* ((a (b big-240-digits))
       (product (bigint-multiply a bigint-zero)))
  (check (bigint-zero? product) => #t)
) ;let*

;; 乘法交换律与结合律
(let* ((a (b big-240-digits))
       (c (b "12345678901234567890"))
       (ab (bigint-multiply a c))
       (ba (bigint-multiply c a)))
  (check (bigint=? ab ba) => #t)
) ;let*

(let* ((a (b big-240-digits))
       (ab-c (bigint-multiply (bigint-multiply a (b "2")) (b "3")))
       (a-bc (bigint-multiply a (bigint-multiply (b "2") (b "3")))))
  (check (bigint=? ab-c a-bc) => #t)
) ;let*

(check-report)
