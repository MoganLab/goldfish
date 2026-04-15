(import (liii check))
(import (liii list))
(import (scheme base))
(check-set-mode! 'report-failed)
;; memv 基本功能测试
(check (memv 1 '(1 2 3)) => '(1 2 3))
(check (memv 2 '(1 2 3)) => '(2 3))
(check (memv 3 '(1 2 3)) => '(3))
(check (memv 4 '(1 2 3)) => #f)
;; 空列表边界测试
(check (memv 0 '()) => #f)
(check (memv #\a '()) => #f)
(check (memv #t '()) => #f)
;; 单元素列表边界测试
(check (memv 42 '(42)) => '(42))
(check (memv 100 '(single)) => #f)
(check (memv 3.14 '(3.14)) => '(3.14))
;; 数值类型边界测试
(check (memv 0 '(0 1 2 3))
  =>
  '(0 1 2 3)
) ;check
(check (memv -42 '(-42 0 42))
  =>
  '(-42 0 42)
) ;check
(check (memv 42 '(-42 0 42)) => '(42))
(check (memv 3.14159 '(2.71 3.14159 1.414))
  =>
  '(3.14159 1.414)
) ;check
(check (memv 1.5 '(1.0 1.5 2.0))
  =>
  '(1.5 2.0)
) ;check
;; 字符类型边界测试
(check (memv #\a '(#\b #\a #\c))
  =>
  '(#\a #\c)
) ;check
(check (memv #\b '(#\c #\b #\a))
  =>
  '(#\b #\a)
) ;check
(check (memv #\newline '(a b c)) => #f)
(check (memv #\space
         '(#\tab #\space #\newline)
       ) ;memv
  =>
  '(#\space #\newline)
) ;check
;; 基本字符测试
(check (memv #\a '(#\b #\a #\c))
  =>
  '(#\a #\c)
) ;check
(check (memv #\A '(#\A #\B #\C))
  =>
  '(#\A #\B #\C)
) ;check
;; 布尔值边界测试
(check (memv #t '(#f #t #f))
  =>
  '(#t #f)
) ;check
(check (memv #f '(#t #f #t))
  =>
  '(#f #t)
) ;check
(check (memv #t '(#f only)) => #f)
;; 符号类型测试（确保eqv?正确处理符号）
(check (memv 'symbol '(other symbol test))
  =>
  '(symbol test)
) ;check
(check (memv '中文符号
         '(关键字 中文符号 测试)
       ) ;memv
  =>
  '(中文符号 测试)
) ;check
;; 重复值处理测试
(check (memv 42 '(10 20 42 42 42))
  =>
  '(42 42 42)
) ;check
(check (memv #\a '(x y z #\a #\a))
  =>
  '(#\a #\a)
) ;check
;; 复数类型边界测试
(check (memv 1.0+2.0i
         '(0.0+1.0i 1.0+2.0i 3.0+4.0i)
       ) ;memv
  =>
  '(1.0+2.0i 3.0+4.0i)
) ;check
(check (memv 0.0 '(1.0+1.0i 2.0+2.0i))
  =>
  #f
) ;check
;; 有理数边界测试
(check (memv 1/2 '(1/3 1/2 2/3))
  =>
  '(1/2 2/3)
) ;check
(check (memv 3/4 '(1/2 1/4)) => #f)
;; 极端边界值测试
(check (memv 2147483647
         '(2147483646 2147483647 2147483648)
       ) ;memv
  =>
  '(2147483647 2147483648)
) ;check
(check (memv -2147483648
         '(-2147483649 -2147483648 -2147483647)
       ) ;memv
  =>
  '(-2147483648 -2147483647)
) ;check
;; 浮点数精度测试
(check (memv 0.0001 '(0.0 0.0001 0.0002))
  =>
  '(0.0001 0.0002)
) ;check
(check (memv 9999.999
         '(9999.998 9999.999 10000.0)
       ) ;memv
  =>
  '(9999.999 10000.0)
) ;check
;; 混合类型列表测试
(check (memv 42 '("string" 42 #\a #t))
  =>
  '(42 #\a #t)
) ;check
(check (memv #\x '(x y z #\x symbol 3.14))
  =>
  '(#\x symbol 3.14)
) ;check
;; 嵌套结构中的顶层查找
(check (memv 1 '((1 2) 3 4)) => #f)
(check (memv 3 '((1 2) 3 4)) => '(3 4))
;; 大列表性能验证
(check (memv 15
         '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
       ) ;memv
  =>
  '(15 16 17 18 19 20)
) ;check
(check (memv 999 (make-list 5 999))
  =>
  '(999 999 999 999 999)
) ;check
(check (memv 20
         (append '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
           '(20)
         ) ;append
       ) ;memv
  =>
  '(20)
) ;check
;; 构造器函数结果测试
(check (memv 2 (list 1 2 3 4 5))
  =>
  '(2 3 4 5)
) ;check
(check (memv 2.5
         (cons 1.0 (cons 2.5 (cons 3.0 '())))
       ) ;memv
  =>
  '(2.5 3.0)
) ;check
;; 向量与字符混合测试
(check (memv 3.14 '(#(1 2) 3.14 #\a "test"))
  =>
  '(3.14 #\a "test")
) ;check
;; 错误参数类型测试
(check-catch 'wrong-type-arg
  (memv 0 "not a list")
) ;check-catch
(check-catch 'wrong-type-arg
  (memv 0 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (memv 0 #t)
) ;check-catch
;; 参数数量错误测试
(check-catch 'wrong-number-of-args
  (memv)
) ;check-catch
(check-catch 'wrong-number-of-args
  (memv 1)
) ;check-catch
(check-catch 'wrong-number-of-args
  (memv 1 '(1 2 3) "extra argument")
) ;check-catch
(check-report)