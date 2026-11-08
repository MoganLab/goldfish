;; string-copy!
;; 把源字符串的区间复制到目标字符串的指定位置。
;;
;; 语法
;; ----
;; (string-copy! to at from)
;; (string-copy! to at from start)
;; (string-copy! to at from start end)
;;
;; 参数
;; ----
;; to : string?
;; 目标字符串（可变）。
;;
;; at : integer?
;; 目标起始下标，0 <= at <= (string-length to)。
;;
;; from : string?
;; 源字符串。
;;
;; start : integer? (可选)
;; 源起始下标（含），默认 0。
;;
;; end : integer? (可选)
;; 源结束下标（不含），默认 (string-length from)。
;;
;; 返回值
;; ----
;; unspecified
;; 以副作用修改 to。要求 (- (string-length to) at) >= (- end start)。
;;
;; 说明
;; ----
;; R7RS 6.7：源与目标重叠时，按“先把源拷入临时字符串再拷入目标”
;; 的语义处理，本实现通过 substring 临时拷贝天然满足该语义。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数类型不正确时抛出错误。
;; out-of-range
;; 当 at/start/end 越界或目标空间不足时抛出错误。

(import (liii check))
(import (liii base))
(import (scheme base))

(check-set-mode! 'report-failed)

;; R7RS 6.7 标准示例
(check
  (let ((a "12345") (b (string-copy "abcde")))
    (string-copy! b 1 a 0 2)
    b
  ) ;let
  => "a12de"
) ;check

;; 省略 start/end：复制整个源串
(check
  (let ((b (string-copy "abcde")))
    (string-copy! b 0 "xy")
    b
  ) ;let
  => "xycde"
) ;check

;; 只省略 end：从 start 复制到源尾
(check
  (let ((b (string-copy "abcde")))
    (string-copy! b 2 "XYZ" 1)
    b
  ) ;let
  => "abYZe"
) ;check

;; 完全覆盖目标
(check
  (let ((b (string-copy "abc")))
    (string-copy! b 0 "xyz")
    b
  ) ;let
  => "xyz"
) ;check

;; 空区间：复制零个字符
(check
  (let ((b (string-copy "abcde")))
    (string-copy! b 2 "xyz" 1 1)
    b
  ) ;let
  => "abcde"
) ;check

;; at 等于目标长度：追加位置，区间为空时不越界
(check
  (let ((b (string-copy "abc")))
    (string-copy! b 3 "" 0 0)
    b
  ) ;let
  => "abc"
) ;check

;; 源与目标同一字符串（重叠）
(check
  (let ((b (string-copy "abcdef")))
    (string-copy! b 0 b 2 5)   ; 取 "cde" 覆盖头部
    b
  ) ;let
  => "cdedef"
) ;check
(check
  (let ((b (string-copy "abcdef")))
    (string-copy! b 3 b 0 3)   ; 取 "abc" 写到尾部
    b
  ) ;let
  => "abcabc"
) ;check

;; 注：s7 的字符串字面量本身可变（实现定义行为），因此本实现
;; 不对字面量目标抛错，与 R7RS "it is an error" 的宽容处理一致。

;; 越界：目标空间不足
(check-catch 'out-of-range (string-copy! (string-copy "abc") 2 "xyz"))
;; 越界：at 超过目标长度
(check-catch 'out-of-range (string-copy! (string-copy "abc") 4 "xy"))
;; 越界：start/end 超出源串
(check-catch 'out-of-range (string-copy! (string-copy "abc") 0 "xy" 0 5))
(check-catch 'out-of-range (string-copy! (string-copy "abc") 0 "xy" 3))

;; 类型错误
(check-catch 'type-error (string-copy! 'sym 0 "xy"))
(check-catch 'type-error (string-copy! (string-copy "abc") #\a "xy"))
(check-catch 'type-error (string-copy! (string-copy "abc") 0 'sym))
(check-catch 'type-error (string-copy! (string-copy "abc") 0 "xy" #\a))

(check-report)
