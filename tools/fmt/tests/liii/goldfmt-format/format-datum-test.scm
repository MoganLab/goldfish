(import (liii check)
  (liii goldfmt-format)
  (liii goldfmt-record)
  (liii raw-string)
) ;import

(check-set-mode! 'report-failed)

;; format-datum
;; 格式化一个 Scheme datum，并返回格式化文本。
;;
;; 语法
;; ----
;; (format-datum datum)
;;
;; 参数
;; ----
;; datum : any?
;; 已经由 `read` 读出的 Scheme datum。
;;
;; 返回值
;; ------
;; string?
;; 返回格式化后的文本。
;;
;; 说明
;; ----
;; 1. `format-datum` 会先调用 `scan` 构造 node 树
;; 2. 内部使用 `format-node` 执行格式化
;; 3. 只返回文本，不返回 positioned node
;;
;; 示例
;; ----
;; (format-datum '(define x 1)) ; => "(define x 1)"

(check (format-datum '(define x 1))
  =>
  "(define x 1)"
) ;check

(check (format-datum 'x) => "x")

(check (format-datum "hello")
  =>
  "\"hello\""
) ;check

(check (format-datum '#(1 2))
  =>
  "#(1 2)"
) ;check

(check (format-datum '(*comment* "hello"))
  =>
  ";; hello"
) ;check

(check (format-datum '(*comment* ""))
  =>
  ";;"
) ;check

;; format-datum+node
;; 格式化一个 Scheme datum，并同时返回带位置信息的新 node。
;;
;; 语法
;; ----
;; (format-datum+node datum)
;;
;; 参数
;; ----
;; datum : any?
;; 已经由 `read` 读出的 Scheme datum。
;;
;; 返回值
;; ------
;; values
;; 返回两个值：
;; 1. text : string? 格式化后的文本
;; 2. positioned-node : env? 或 atom? 带有布局位置信息的新 node
;;
;; 说明
;; ----
;; `format-datum+node` 是 `format-datum` 的信息保留版本，适合测试和后续需要定位信息的功能。

(call-with-values (lambda ()
                    (format-datum+node '(define (f x) (+ x 1))
                    ) ;format-datum+node
                  ) ;lambda
  (lambda (text node)
    (check text
      =>
      (&- #""
                 (define (f x)
                   (+ x 1)
                 ) ;define
                 ""
      ) ;&-
    ) ;check
    (check (env-indent node) => 0)
    (check (env-left-line node) => 1)
    (check (env-right-line node) => 3)
  ) ;lambda
) ;call-with-values

;; 由于 max-inline-length=40，这个表达式可以内联
(check (format-datum '(define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))
       ) ;format-datum
  =>
  (&- #""
               (define (factorial n)
                 (if (= n 0) 1 (* n (factorial (- n 1))))
               ) ;define
               ""
  ) ;&-
) ;check

(check (format-datum '(if (very-long-predicate-name x) (compute-true-branch x) (compute-false-branch x))
       ) ;format-datum
  =>
  (&- #""
            (if (very-long-predicate-name x)
              (compute-true-branch x)
              (compute-false-branch x)
            ) ;if
            ""
  ) ;&-
) ;check

(check (format-datum '(let ((x (begin (display "computing") (compute-x arg1))) (y 20)) (+ x y))
       ) ;format-datum
  =>
  (&- #""
             (let ((x (begin (display "computing") (compute-x arg1))) (y 20))
               (+ x y)
             ) ;let
             ""
  ) ;&-
) ;check

;; 阶段1: quote 格式测试
;; 单引号应该输出为 'xxx 形式，而不是 (quote xxx) 或 (#_quote xxx)
(check (format-datum '(quote x))
  =>
  "'x"
) ;check

(check (format-datum '(quote (a b c)))
  =>
  "'(a b c)"
) ;check

(check (format-datum '(set! x (#_quote value)))
  =>
  "(set! x 'value)"
) ;check

(check (format-datum '(f (#_quote a) (#_quote b))
       ) ;format-datum
  =>
  "(f 'a 'b)"
) ;check

(check (format-datum '(#_quote symbol))
  =>
  "'symbol"
) ;check

(check (format-datum '(#_quote (a b c)))
  =>
  "'(a b c)"
) ;check

;; 测试嵌套 quote 形式 '(quote define)
;; '(quote define) 读取为 (#_quote (quote define))
;; 内部的 (quote define) 是普通列表，不应被压缩
;; 所以输出保持为 '(quote define)
(check (format-datum '(#_quote (quote define)))
  =>
  "'(quote define)"
) ;check

;; 测试 '(quote x) 的格式化
(check (format-datum '(#_quote (quote x)))
  =>
  "'(quote x)"
) ;check

;; quasiquote 内部形式应该还原为 ` , ,@ 语法
(check (format-datum (read (open-input-string "`(a ,b)"))
       ) ;format-datum
  =>
  "`(a ,b)"
) ;check

(check (format-datum (read (open-input-string "`(a ,@b)"))
       ) ;format-datum
  =>
  "`(a ,@b)"
) ;check

;; fill 模式：全部子元素为 atom 且数量 >= 4 且无法整体内联时，
;; 元素填充到同一行（不超过 max-inline-length=80），续行缩进 parent+2，
;; 闭合括号保持 ) ;tag 独占一行。
(check (format-datum (cons 'list (iota 30 1)))
  =>
  (&- #""
       (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
         29 30
       ) ;list
       ""
  ) ;&-
) ;check

;; 只有 3 个 atom 子元素时不触发 fill，保持一行一个
(check (format-datum (list 'f (string->symbol (make-string 78 #\a)) 'b 'c))
  =>
  (&- #""
       (f aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
         b
         c
       ) ;f
       ""
  ) ;&-
) ;check

;; 子元素 >= 4 但含有子列表时不触发 fill，保持一行一个
(check (format-datum (list 'list (string->symbol (make-string 74 #\a)) 1 2 '(+ 3 4) 5))
  =>
  (&- #""
       (list aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
         1
         2
         (+ 3 4)
         5
       ) ;list
       ""
  ) ;&-
) ;check

;; 引用数据（reader 路径）：无符号 head 的纯 atom 列表也触发 fill，
;; 续行与第一个元素对齐，闭合括号在行尾
(check (format-datum (list 'quote (iota 30 1)))
  =>
  (&- #""
       '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
         30)
       ""
  ) ;&-
) ;check

(check-report)
