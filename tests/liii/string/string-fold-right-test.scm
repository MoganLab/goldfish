(import (liii check)
  (liii error)
  (liii string)
  (srfi srfi-13)
) ;import

;; string-fold-right
;; 通过从右到左的顺序遍历字符串字符，将给定过程应用于每个字符和累加器值。
;;
;; 语法
;; ----
;; (string-fold-right proc knil s)
;; (string-fold-right proc knil s start)
;; (string-fold-right proc knil s start end)
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
;; string-fold-right与string-fold的主要区别在于遍历顺序：
;; - string-fold: 从左到右（low indices to high）
;; - string-fold-right: 从右到左（high indices to low）
;; 与常规fold类似，fold-right有时可以提供更自然的右结合构建方式。
;; 常用于需要逆序处理字符串的场景。
;;
;; 错误处理
;; ----
;; type-error 当proc不是procedure?类型时
;; wrong-type-arg 当s不是字符串类型时
;; out-of-range 当start/end超出字符串索引范围或start > end时

;; 基本功能测试 - 空字符串
(check (string-fold-right (lambda (c acc) (+ acc 1))
         0
         ""
       ) ;string-fold-right
  =>
  0
) ;check

;; 基本功能测试 - 简单累加
(check (string-fold-right (lambda (c acc) (+ acc 1))
         0
         "hello"
       ) ;string-fold-right
  =>
  5
) ;check

;; 字符收集测试 - 注意顺序与fold相反
(check (string-fold-right cons '() "abc")
  =>
  '(#\a #\b #\c)
) ;check

;; 内容处理测试 - 字符连接方向验证
(check (string-fold-right (lambda (c acc)
                            (string-append acc (string c))
                          ) ;lambda
         ""
         "abc"
       ) ;string-fold-right
  =>
  "cba"
) ;check

;; 统计分析测试
(check (string-fold-right (lambda (c acc)
                            (if (char=? c #\l) (+ acc 1) acc)
                          ) ;lambda
         0
         "hello world"
       ) ;string-fold-right
  =>
  3
) ;check

;; ASCII码累加求和
(check (string-fold-right (lambda (c total)
                            (+ total (char->integer c))
                          ) ;lambda
         0
         "AB"
       ) ;string-fold-right
  =>
  131
) ;check

;; start/end 范围参数测试
(check (string-fold-right (lambda (c acc) (+ acc 1))
         0
         "hello"
         1
         4
       ) ;string-fold-right
  =>
  3
) ;check
(check (string-fold-right cons
         '()
         "abcdef"
         2
         5
       ) ;string-fold-right
  =>
  '(#\c #\d #\e)
) ;check

;; 边界条件测试 - single character
(check (string-fold-right (lambda (c acc) (+ acc 1))
         0
         "a"
       ) ;string-fold-right
  =>
  1
) ;check

;; 边界条件测试 - range equals string length
(check (string-fold-right (lambda (c acc) (+ acc 1))
         0
         "test"
         0
         4
       ) ;string-fold-right
  =>
  4
) ;check

;; 极限空范围测试
(check (string-fold-right (lambda (c acc) (+ acc 1))
         0
         "test"
         2
         2
       ) ;string-fold-right
  =>
  0
) ;check

;; Unicode字符测试
(check (string-fold-right (lambda (c acc) (+ acc 1))
         0
         "测试"
       ) ;string-fold-right
  =>
  (string-length "测试")
) ;check

;; 反向构建测试
(check (string-fold-right (lambda (c acc)
                            (string-append acc
                              (string (char-downcase c))
                            ) ;string-append
                          ) ;lambda
         ""
         "XYZ"
       ) ;string-fold-right
  =>
  "zyx"
) ;check

;; === 错误处理测试 ===

;; 参数类型错误测试
(check-catch 'type-error
  (string-fold-right 123 0 "hello")
) ;check-catch
(check-catch 'type-error
  (string-fold-right (lambda (c acc) (+ acc 1))
    0
    123
  ) ;string-fold-right
) ;check-catch
(check-catch 'type-error
  (string-fold-right "not-a-proc"
    0
    "hello"
  ) ;string-fold-right
) ;check-catch

;; 范围越界测试
(check-catch 'out-of-range
  (string-fold-right (lambda (c acc) (+ acc 1))
    0
    "hello"
    -1
  ) ;string-fold-right
) ;check-catch
(check-catch 'out-of-range
  (string-fold-right (lambda (c acc) (+ acc 1))
    0
    "hello"
    0
    6
  ) ;string-fold-right
) ;check-catch
(check-catch 'out-of-range
  (string-fold-right (lambda (c acc) (+ acc 1))
    0
    "hello"
    3
    2
  ) ;string-fold-right
) ;check-catch
(check-catch 'out-of-range
  (string-fold-right (lambda (c acc) (+ acc 1))
    0
    ""
    1
    2
  ) ;string-fold-right
) ;check-catch

(check-report)
