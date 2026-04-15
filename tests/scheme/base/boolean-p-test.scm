(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; boolean?
;; 判断一个对象是否为布尔值。
;;
;; 语法
;; ----
;; (boolean? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果obj是布尔值（即#t或#f）则返回#t，否则返回#f。
;;
;; 说明
;; ----
;; 1. 用于确定对象是否为布尔值类型
;; 2. 能够正确识别标准的布尔值#t和#f
;; 3. 返回布尔值，便于在类型判断中使用
;; 4. 对所有非布尔对象均返回#f
;;
;; 示例
;; ----
;; (boolean? #t) => #t
;; (boolean? #f) => #t
;; (boolean? 123) => #f
;; (boolean? "true") => #f
;; (boolean? 'symbol) => #f
;;
;; 边界情况
;; --------
;; - 只识别 #t 和 #f
;; - 所有非布尔类型的参数都返回 #f
;; - 可以识别由其他表达式返回的布尔值
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 当参数数量不为1时抛出错误。
;; boolean? 基础测试
(check (boolean? #t) => #t)
(check (boolean? #f) => #t)
;; boolean? 非布尔类型测试
(check (boolean? 0) => #f)
(check (boolean? 1) => #f)
(check (boolean? -1) => #f)
(check (boolean? 3.14) => #f)
(check (boolean? #\a) => #f)
(check (boolean? "true") => #f)
(check (boolean? "false") => #f)
(check (boolean? "#t") => #f)
(check (boolean? 'true) => #f)
(check (boolean? 'false) => #f)
(check (boolean? 'symbol) => #f)
(check (boolean? '(1 2 3)) => #f)
(check (boolean? '()) => #f)
(check (boolean? #()) => #f)
;; boolean? 复杂类型测试
(check (boolean? (lambda (x) x)) => #f)
(check (boolean? "string") => #f)
(check (boolean? 123.456) => #f)
(check (boolean? #\space) => #f)
(check (boolean? #\newline) => #f)
;; boolean? 布尔返回值测试
(check (boolean? (eq? 1 1)) => #t)
(check (boolean? (= 1 2)) => #t)
(check (boolean? (> 3 2)) => #t)
(check (boolean? (< 1 2)) => #t)
(check (boolean? (zero? 0)) => #t)
(check (boolean? (null? '())) => #t)
(check (boolean? (null? '(1))) => #t)
;; boolean? 特殊边界测试
(check (boolean? #t) => #t)
(check (boolean? #f) => #t)
(check (boolean? 'nil) => #f)
(check (boolean? 't) => #f)
(check (boolean? 'f) => #f)
;; boolean? 与布尔运算结合测试
(check (boolean? (not #t)) => #t)
(check (boolean? (not #f)) => #t)
(check (boolean? (not 123)) => #t)
(check (boolean? (and #t #f)) => #t)
(check (boolean? (or #t #f)) => #t)
(check (boolean? (boolean=? #t #t))
  =>
  #t
) ;check
;; 类型判断一致性测试
(check (boolean? (boolean? #t)) => #t)
(check (boolean? (boolean? #f)) => #t)
(check (boolean? (boolean? 123)) => #t)
(check (boolean? (string? "hello"))
  =>
  #t
) ;check
(check (boolean? (integer? 42)) => #t)
;; 边界类型测试
(check (boolean? 0) => #f)
(check (boolean? 1.0+2.0i) => #f)
(check (boolean? +inf.0) => #f)
(check (boolean? -inf.0) => #f)
(check (boolean? +nan.0) => #f)
;; 错误处理测试
(check-catch 'wrong-number-of-args
  (boolean?)
) ;check-catch
(check-catch 'wrong-number-of-args
  (boolean? #t #f)
) ;check-catch
(check-catch 'wrong-number-of-args
  (boolean? 1 2 3)
) ;check-catch
(check-report)