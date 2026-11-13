;;; compat/bignum.scm - Goldfish (r7rs+bignum) 与 Chez Scheme bignum 行为对比脚本
;;;
;;; 用法：
;;;   scheme --script compat/bignum.scm > /tmp/chez.out
;;;   bin/gf --mode r7rs+bignum compat/bignum.scm > /tmp/gf.out
;;;   diff -u /tmp/chez.out /tmp/gf.out
;;;
;;; 注释约定：
;;;   [一致]   ：bin/gf (--mode r7rs+bignum) 与 scheme 输出完全一致
;;;   [不一致] ：bin/gf 与 scheme 行为存在差异（附双方实际输出）

(define-syntax check
  (syntax-rules ()
    ((_ label expr)
     (begin
       (display label)
       (display " => ")
       (let ((res (guard (e (else 'ERR)) expr)))
         (write res))
       (newline)))))

;; 1. 谓词
(check "number?" (number? 11111111111111111111111111111111111))       ; [一致] #t
(check "complex?" (complex? 11111111111111111111111111111111111))     ; [一致] #t
(check "real?" (real? 11111111111111111111111111111111111))           ; [一致] #t
(check "rational?" (rational? 11111111111111111111111111111111111))   ; [一致] #t
(check "integer?" (integer? 11111111111111111111111111111111111))     ; [一致] #t
(check "exact?" (exact? 11111111111111111111111111111111111))         ; [一致] #t
(check "inexact?" (inexact? 11111111111111111111111111111111111))     ; [一致] #f
(check "zero?" (zero? 11111111111111111111111111111111111))           ; [一致] #f
(check "positive?" (positive? 11111111111111111111111111111111111))   ; [一致] #t
(check "negative?" (negative? -11111111111111111111111111111111111))  ; [一致] #t
(check "odd?" (odd? 11111111111111111111111111111111111))             ; [一致] #t
(check "even?" (even? 11111111111111111111111111111111112))           ; [一致] #t

;; 2. int64 边界与升降级
(check "add-overflow" (+ 9223372036854775807 1))                      ; [一致] 9223372036854775808
(check "sub-demote" (- (+ 9223372036854775807 1) 1))                  ; [一致] 9223372036854775807
(check "sub-underflow" (- -9223372036854775808 1))                    ; [一致] -9223372036854775809
(check "neg-int64-min" (- -9223372036854775808))                      ; [一致] 9223372036854775808
(check "abs-int64-min" (abs -9223372036854775808))                    ; [一致] 9223372036854775808
(check "quot-int64-min" (quotient -9223372036854775808 -1))           ; [一致] 9223372036854775808
(check "div-int64-min" (/ -9223372036854775808 -1))                   ; [一致] 9223372036854775808
(check "mul-int64-min" (* -9223372036854775808 -1))                   ; [一致] 9223372036854775808
(check "mul-int64-max" (* 9223372036854775807 9223372036854775807))   ; [一致] 85070591730234615847396907784232501249
(check "eqv-big" (eqv? (+ 9223372036854775807 1) 9223372036854775808)) ; [一致] #t
(check "eqv-demote" (eqv? (- (+ 9223372036854775807 1) 1) 9223372036854775807)) ; [一致] #t

;; 3. 比较（精确 vs 非精确 / +inf.0）
(check "eq-big-float-same" (= 100000000000000000000 1e20))            ; [一致] #t
(check "eq-big-float-diff" (= 100000000000000000001 1e20))            ; [一致] #f
(check "gt-big-float-diff" (> 100000000000000000001 1e20))            ; [一致] #t
(check "lt-big-big" (< 100000000000000000000 100000000000000000001))  ; [一致] #t
(check "eq-big-inf" (= (expt 10 400) +inf.0))                         ; [一致] #f
(check "lt-big-inf" (< (expt 10 400) +inf.0))                         ; [一致] #t
(check "max-big-float" (max 100000000000000000001 1e20))              ; [不一致(显示格式)] gf: 1e+20 | scheme: 1e20
(check "min-big-float" (min 100000000000000000001 1e20))              ; [不一致(显示格式)] gf: 1e+20 | scheme: 1e20
(check "exact?-max-inexact" (exact? (max 100000000000000000000 1.0))) ; [一致] #f

;; 4. 除法与有理数混合运算
(check "div-big-3" (/ 100000000000000000000 3))                       ; [不一致] gf: 33333333333333330000.0 | scheme: 100000000000000000000/3
(check "div-1-big" (/ 1 100000000000000000000))                       ; [不一致] gf: 1e-20 | scheme: 1/100000000000000000000
(check "div-recip-big" (/ 100000000000000000000))                     ; [不一致] gf: 1e-20 | scheme: 1/100000000000000000000
(check "exact?-div-big-3" (exact? (/ 100000000000000000000 3)))       ; [不一致] gf: #f | scheme: #t
(check "mul-div-big-3" (* (/ 100000000000000000000 3) 3))             ; [不一致] gf: 1e+20 | scheme: 100000000000000000000
(check "add-big-rat" (+ 100000000000000000000 1/2))                   ; [不一致] gf: 1e+20 | scheme: 200000000000000000001/2
(check "sub-big-rat" (- 100000000000000000000 1/2))                   ; [不一致] gf: 1e+20 | scheme: 199999999999999999999/2
(check "mul-big-rat-2" (* 100000000000000000000 1/2))                 ; [一致] 50000000000000000000
(check "mul-big-rat-3" (* 100000000000000000000 1/3))                 ; [不一致] gf: 33333333333333330000.0 | scheme: 100000000000000000000/3
(check "div-big-rat" (/ 100000000000000000000 1/2))                   ; [一致] 200000000000000000000
(check "div-rat-big" (/ 1/2 100000000000000000000))                   ; [不一致] gf: 5e-21 | scheme: 1/200000000000000000000

;; 5. quotient / remainder / modulo
(check "quotient-big" (quotient 100000000000000000005 10))            ; [一致] 10000000000000000000
(check "remainder-neg-pos" (remainder -100000000000000000005 10))     ; [一致] -5
(check "modulo-neg-pos" (modulo -100000000000000000005 10))           ; [一致] 5
(check "modulo-pos-neg" (modulo 100000000000000000005 -10))           ; [一致] -5
(check "modulo-neg-neg" (modulo -100000000000000000005 -10))          ; [一致] -5
(check "remainder-int64-min" (remainder -9223372036854775808 -1))     ; [一致] 0
(check "modulo-int64-min" (modulo -9223372036854775808 -1))           ; [一致] 0
(check "modulo-big-float" (modulo 100000000000000000005 3.0))         ; [一致] 0.0

;; 6. gcd / lcm / expt
(check "gcd-0" (gcd))                                                 ; [一致] 0
(check "lcm-0" (lcm))                                                 ; [一致] 1
(check "gcd-neg-1" (gcd -100000000000000000000))                      ; [一致] 100000000000000000000
(check "lcm-neg-1" (lcm -100000000000000000000))                      ; [一致] 100000000000000000000
(check "gcd-big-float" (gcd 100000000000000000000 1.0))               ; [一致] 1.0
(check "lcm-big-float" (lcm 100000000000000000000 2.0))               ; [一致(显示格式)] gf: 1e+20 | scheme: 1e20
(check "gcd-multi" (gcd 12 18 100000000000000000000))                 ; [一致] 2
(check "lcm-big" (lcm 100000000000000000000 150000000000000000000))   ; [一致] 300000000000000000000
(check "expt-0-0" (expt 0 0))                                         ; [一致] 1
(check "expt-0-big" (expt 0 100000000000000000000))                   ; [一致] 0
(check "expt-1-big" (expt 1 100000000000000000000))                   ; [一致] 1
(check "expt--1-even" (expt -1 100000000000000000000))             ; [一致] 1
(check "expt--1-odd" (expt -1 100000000000000000001))              ; [一致] -1
(check "expt-2--100" (expt 2 -100))                                   ; [不一致] gf: 7.888609052210118e-31 | scheme: 1/1267650600228229401496703205376
(check "expt-2-100" (expt 2 100))                                     ; [一致] 1267650600228229401496703205376

;; 7. 取整与分子分母
(check "floor-big" (floor 100000000000000000000))                     ; [一致] 100000000000000000000
(check "ceiling-big" (ceiling 100000000000000000000))                 ; [一致] 100000000000000000000
(check "truncate-big" (truncate 100000000000000000000))               ; [一致] 100000000000000000000
(check "round-big" (round 100000000000000000000))                     ; [一致] 100000000000000000000
(check "numerator-big" (numerator 100000000000000000000))             ; [一致] 100000000000000000000
(check "denominator-big" (denominator 100000000000000000000))         ; [一致] 1

;; 8. 转换与字符串
(check "inexact-big" (inexact 100000000000000000000))                 ; [不一致(显示格式)] gf: 1e+20 | scheme: 1e20
(check "exact-1e20" (exact 1e20))                                     ; [一致] 100000000000000000000
(check "exact-1e30" (exact 1e30))                                     ; [一致] 1000000000000000019884624838656
(check "inexact-1e400" (inexact (expt 10 400)))                       ; [一致] +inf.0
(check "num->str-10" (number->string 123456789012345678901234567890 10)) ; [一致] "123456789012345678901234567890"
(check "num->str-2" (number->string 123456789012345678901234567890 2))   ; [一致] "1100011101110100100001111111101101100001101110011111000001110111001001110001111110000101011010010"
(check "num->str-8" (number->string 123456789012345678901234567890 8))   ; [一致] "143564417755415637016711617605322"
(check "num->str-16" (number->string 123456789012345678901234567890 16)) ; [不一致(大小写)] gf: "18ee90ff6c373e0ee4e3f0ad2" | scheme: "18EE90FF6C373E0EE4E3F0AD2"
(check "str->num-10" (string->number "123456789012345678901234567890")) ; [一致] 123456789012345678901234567890
(check "str->num-#x" (string->number "#x1234567890abcdef1234567890"))    ; [一致] 1442304682740643783150283421840
(check "str->num-16" (string->number "1234567890abcdef1234567890" 16))   ; [一致] 1442304682740643783150283421840
(check "str->num-#e1e20" (string->number "#e1e20"))                   ; [一致] 100000000000000000000
(check "str->num-#i-big" (string->number "#i123456789012345678901234567890")) ; [一致(显示格式)] gf: 1.2345678901234568e+29 | scheme: 1.2345678901234568e29
(check "str->num-rat" (string->number "200000000000000000000/2"))     ; [一致] 100000000000000000000
