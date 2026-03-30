(import (liii check)
        (liii string)
) ;import

;; string-for-each-index
;; 遍历字符串的每个字符及其索引位置。
;;
;; 语法
;; ----
;; (string-for-each-index proc str)
;; (string-for-each-index proc str start)
;; (string-for-each-index proc str start end)
;;
;; 参数
;; ----
;; proc : procedure?
;; 一个函数，接收三个参数：索引、字符和累加器，返回新的累加器值。
;;
;; str : string?
;; 要处理的源字符串。
;;
;; start : integer? 可选
;; 遍历的起始位置索引（包含），默认为0。
;;
;; end : integer? 可选
;; 遍历的结束位置索引（不包含），默认为字符串长度。
;;
;; 返回值
;; ----
;; list
;; 返回proc应用于所有字符后的累加器结果列表。
;;
;; 注意
;; ----
;; string-for-each-index会同时提供字符的索引位置和字符本身。
;; 常用于需要知道字符位置的字符串处理。
;;
;; 错误处理
;; ----
;; type-error 当proc不是procedure?类型时
;; wrong-type-arg 当str不是字符串类型时
;; out-of-range 当start/end超出字符串索引范围时

(check
  (string-for-each-index
    (lambda (i c acc)
      (cons (list i c) acc)
    ) ;lambda
    "hello"
  ) ;string-for-each-index
  => '((0 #\h) (1 #\e) (2 #\l) (3 #\l) (4 #\o))
) ;check

(check
  (string-for-each-index
    (lambda (i c acc)
      (cons (list i c) acc)
    ) ;lambda
    (substring "hello" 1 4)
  ) ;string-for-each-index
  => '((0 #\e) (1 #\l) (2 #\l))
) ;check

(check
  (list->string
    (reverse
      (string-for-each-index
        (lambda (i c acc)
          (cons c acc)
        ) ;lambda
        "hello"
      ) ;string-for-each-index
    ) ;reverse
  ) ;list->string
  => "olleh"
) ;check

(check
  (string-for-each-index
    (lambda (i c acc)
      (cons (list i c) acc)
    ) ;lambda
    ""
  ) ;string-for-each-index
  => '()
) ;check

(check-catch 'out-of-range
  (string-for-each-index
   (lambda (i c) (display c))
   "hello" 6
  ) ;string-for-each-index
) ;check-catch

(check-catch 'out-of-range
  (string-for-each-index
   (lambda (i c) (display c))
   "hello" 0 6
  ) ;string-for-each-index
) ;check-catch

(check-catch 'out-of-range
  (string-for-each-index
   (lambda (i c) (display c))
   "hello" 3 2
  ) ;string-for-each-index
) ;check-catch

(check-catch 'type-error
  (string-for-each-index
   (lambda (i c) (display c))
   123
  ) ;string-for-each-index
) ;check-catch

(check-report)
