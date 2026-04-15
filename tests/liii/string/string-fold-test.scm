(import (liii check)
  (liii error)
  (liii string)
  (srfi srfi-13)
) ;import

;; string-fold
;; 通过从左到右的顺序遍历字符串字符，将给定过程应用于每个字符和累加器值。
;;
;; 语法
;; ----
;; (string-fold proc knil s)
;; (string-fold proc knil s start)
;; (string-fold proc knil s start end)
;;
;; 参数
;; ----
;; proc : procedure?
;;   一个函数，接收两个参数：当前字符和当前累加器值，返回新的累加器值。
;;
;; knil : any
;;   初始累加器值。
;;
;; s : string?
;;   要遍历的源字符串。
;;
;; start : integer? 可选
;;   遍历的起始位置（包含），默认为0。
;;
;; end : integer? 可选
;;   遍历的结束位置（不包含），默认为字符串长度。
;;
;; 返回值
;; ----
;; any
;;   最后一个累加器值，即将proc应用于所有相关字符后的结果。
;;
;; 注意
;; ----
;; string-fold是一种累加器函数，用于从左到右处理字符串字符。
;; 常用于字符串统计、转换累加或逐步构建复杂结果。
;; 空字符串直接返回初始累加器值knil。
;; 支持可选的start/end参数限定处理范围。
;;
;; 错误处理
;; ----
;; type-error 当proc不是procedure?类型时
;; wrong-type-arg 当s不是字符串类型时
;; out-of-range 当start/end超出字符串索引范围或start > end时

;; 基本功能测试 - 空字符串
(check (string-fold (lambda (c acc) (+ acc 1))
         0
         ""
       ) ;string-fold
  =>
  0
) ;check

;; 基本功能测试 - 简单累加
(check (string-fold (lambda (c acc) (+ acc 1))
         0
         "hello"
       ) ;string-fold
  =>
  5
) ;check

;; 字符收集测试
(check (string-fold cons '() "abc")
  =>
  '(#\c #\b #\a)
) ;check

;; 内容处理测试 - 字符连接方向验证
(check (string-fold (lambda (c acc)
                      (string-append acc (string c))
                    ) ;lambda
         ""
         "abc"
       ) ;string-fold
  =>
  "abc"
) ;check

;; 统计分析测试
(check (string-fold (lambda (c acc)
                      (if (char=? c #\a) (+ acc 1) acc)
                    ) ;lambda
         0
         "banana"
       ) ;string-fold
  =>
  3
) ;check

;; ASCII码累加求和
(check (string-fold (lambda (c total)
                      (+ total (char->integer c))
                    ) ;lambda
         0
         "AB"
       ) ;string-fold
  =>
  131
) ;check

;; 字符过滤 - 数字
(check (string-fold (lambda (c acc)
                      (if (char-numeric? c) (cons c acc) acc)
                    ) ;lambda
         '()
         "a1b2c3"
       ) ;string-fold
  =>
  '(#\3 #\2 #\1)
) ;check

;; 字符分类统计
(check (string-fold (lambda (c counts)
                      (cond ((char-alphabetic? c)
                             (list (+ (car counts) 1)
                               (cadr counts)
                               (caddr counts)
                             ) ;list
                            ) ;
                            ((char-numeric? c)
                             (list (car counts)
                               (+ (cadr counts) 1)
                               (caddr counts)
                             ) ;list
                            ) ;
                            (else (list (car counts)
                                    (cadr counts)
                                    (+ (caddr counts) 1)
                                  ) ;list
                            ) ;else
                      ) ;cond
                    ) ;lambda
         '(0 0 0)
         "hello123!"
       ) ;string-fold
  =>
  '(5 3 1)
) ;check

;; start/end 范围参数测试
(check (string-fold (lambda (c acc) (+ acc 1))
         0
         "hello"
         1
         4
       ) ;string-fold
  =>
  3
) ;check
(check (string-fold cons '() "abcdef" 2 5)
  =>
  '(#\e #\d #\c)
) ;check

;; 边界条件测试 - single character
(check (string-fold (lambda (c acc) (+ acc 1))
         0
         "a"
       ) ;string-fold
  =>
  1
) ;check

;; 边界条件测试 - range equals string length
(check (string-fold (lambda (c acc) (+ acc 1))
         0
         "test"
         0
         4
       ) ;string-fold
  =>
  4
) ;check

;; 极限空范围测试
(check (string-fold (lambda (c acc) (+ acc 1))
         0
         "test"
         2
         2
       ) ;string-fold
  =>
  0
) ;check

;; 复杂lambda计算测试
(check (string-fold (lambda (c acc)
                      (+ acc
                        (* (char->integer c) (char->integer c))
                      ) ;+
                    ) ;lambda
         0
         "AB"
       ) ;string-fold
  =>
  8581
) ;check

(check (string-fold (lambda (c acc)
                      (max acc (char->integer c))
                    ) ;lambda
         0
         "ABC"
       ) ;string-fold
  =>
  67
) ;check

;; Unicode字符测试
(check (string-fold (lambda (c acc) (+ acc 1))
         0
         "中文"
       ) ;string-fold
  =>
  (string-length "中文")
) ;check

;; 反向构建测试
(check (string-fold (lambda (c acc)
                      (string-append acc
                        (string (char-upcase c))
                      ) ;string-append
                    ) ;lambda
         ""
         "abc"
       ) ;string-fold
  =>
  "ABC"
) ;check

;; 多类型累加器 - hand calculation: 104+101+108+108+111 = 532 for "hello"
(check (string-fold (lambda (c acc)
                      (+ acc (char->integer c))
                    ) ;lambda
         0
         "hello"
       ) ;string-fold
  =>
  532
) ;check

;; === 错误处理测试 ===

;; 参数类型错误测试
(check-catch 'type-error
  (string-fold 123 0 "hello")
) ;check-catch
(check-catch 'type-error
  (string-fold (lambda (c acc) (+ acc 1))
    0
    123
  ) ;string-fold
) ;check-catch
(check-catch 'type-error
  (string-fold "not-a-proc" 0 "hello")
) ;check-catch

;; 范围越界测试
(check-catch 'out-of-range
  (string-fold (lambda (c acc) (+ acc 1))
    0
    "hello"
    -1
  ) ;string-fold
) ;check-catch
(check-catch 'out-of-range
  (string-fold (lambda (c acc) (+ acc 1))
    0
    "hello"
    0
    6
  ) ;string-fold
) ;check-catch
(check-catch 'out-of-range
  (string-fold (lambda (c acc) (+ acc 1))
    0
    "hello"
    3
    2
  ) ;string-fold
) ;check-catch
(check-catch 'out-of-range
  (string-fold (lambda (c acc) (+ acc 1))
    0
    ""
    1
    2
  ) ;string-fold
) ;check-catch

(check-report)
