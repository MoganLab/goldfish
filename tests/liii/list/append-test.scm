(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; append 函数测试
;;
;; 连接多个列表并返回新列表。
;;
;; 语法
;; ----
;; (append list ...)
;;
;; 参数
;; ----
;; list : list?
;; 零个或多个要连接的列表。
;;
;; 返回值
;; ----
;; list
;; 一个新的列表，包含所有输入列表的元素。
;;
;; 说明
;; ----
;; append函数接受多个列表作为参数，返回一个新列表，其中包含所有输入列表的元素。
;; 最后一个参数可以是任意对象，此时返回的是一个点对（dotted list）。

; 基本功能测试
(check (append) => '())
(check (append '()) => '())
(check (append '(1 2 3)) => '(1 2 3))
(check (append '(1 2) '(3 4)) => '(1 2 3 4))
(check (append '(a b) '(c d) '(e f)) => '(a b c d e f))
(check (append '() '()) => '())
(check (append '() '(1) '()) => '(1))

; 嵌套列表测试
(check (append '((1 2)) '((3 4))) => '((1 2) (3 4)))
(check (append '(a b) '((c d) (e f))) => '(a b (c d) (e f)))

; 空列表处理
(check (append '() '(a b c)) => '(a b c))
(check (append '(a b c) '()) => '(a b c))
(check (append '() '() '(1 2) '()) => '(1 2))

; 不同类型元素测试
(check (append '(1 2) '("hello" 'symbol)) => '(1 2 "hello" 'symbol))
(check (append '(#t #f) '(42)) => '(#t #f 42))
(check (append '(1 2.5) '(#\a #\b)) => '(1 2.5 #\a #\b))

; 最后一个参数为点对（dotted list）
(check (append '(a b) 'c) => '(a b . c))
(check (append '(1 2) '(3 . 4)) => '(1 2 3 . 4))
(check (append '() 'x) => 'x)

; 单参数测试
; 注意：单参数append可能返回原列表或副本，取决于实现
(let ((original '(a b c)))
  (let ((appended (append original)))
    (check-true (list? appended))
    (check (length appended) => (length original))
    (check appended => '(a b c))
  ) ;let
) ;let

; 不修改原始列表测试
(let ((l1 '(1 2 3)))
  (let ((l2 '(4 5 6)))
    (let ((result (append l1 l2)))
      (set-car! result 99)
      (check l1 => '(1 2 3))
      (check l2 => '(4 5 6))
      (check result => '(99 2 3 4 5 6))
    ) ;let
  ) ;let
) ;let

; 复杂组合测试
(check (append '(1) '(2) '(3) '(4) '(5)) => '(1 2 3 4 5))
(check (append '(a b) '() '(c) '() '(d e)) => '(a b c d e))

; Bug 复现测试：使用 append 构建长列表时结构损坏问题
; 测试：使用 map + member（原始场景）
(let* ((entries (map (lambda (i) (cons (number->string i) (list "lib"))) (iota 600)))
       (result
         (let loop ((entries entries)
                    (visible '()))
           (if (null? entries)
               visible
               (let* ((entry (car entries))
                      (function-name (car entry)))
                 (loop (cdr entries)
                       (if (not (member function-name visible))
                           (append visible (list function-name))
                           visible)))))))
  (check-true (list? result))
  (check (length result) => 600))

(check-report)
